{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Either
import Data.Maybe
import System.FilePath ((</>))
import System.Directory
import System.Locale (defaultTimeLocale)
import System.Posix.IO (stdInput, fdReadBuf)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Misc.TrayManager
import qualified Data.ByteString.Internal as B (createAndTrim)
import Data.ByteString.UTF8 (toString)

import StrutProperties
import VerticalBar

debug = False

strutProperties :: Int -> Rectangle -> StrutProperties
strutProperties bh (Rectangle mX mY mW _) =
    (0, 0, sH, 0, 0, 0, 0, 0, sX, sX+sW, 0,   0)
  where sX = mX
        sW = mW - 1
        sH = bh + mY

-- simple systray 
systrayNew screen = do
    box <- hBoxNew False 3
    trayManager <- trayManagerNew
    _ <- trayManagerManageScreen trayManager screen
    void $ trayManager `on` trayIconAdded $ \widget -> do
        widgetShowAll widget
        boxPackStart box widget PackNatural 0
    return box

-- simple clock
clockNew = getCurrentTimeZone >>= createWidget
  where createWidget tz = do
            (updateIn, t) <- getUpdatedText
            label         <- labelNew Nothing
            labelSetMarkup label ("<b>" ++ t ++ "</b>")
            _             <- timeoutAdd (doUpdate label) updateIn
            return label
          where -- return the time before next update in ms and the formatted string
                getUpdatedText = do
                    current <- getCurrentTime
                    let local = utcToZonedTime tz current
                    let diffTime = floor (toRational (utctDayTime current)) `mod` 60
                    return ((60 - diffTime) * 1000, formatTime defaultTimeLocale "%F %R" local)
                doUpdate widget = do
                    (updateIn, t) <- getUpdatedText
                    labelSetMarkup widget ("<b>" ++ t ++ "</b>")
                    _ <- timeoutAdd (doUpdate widget) updateIn
                    return False

-- | xmonad reporting label
xmonadLogNew inFd = do
    box   <- hBoxNew False 1
    label <- labelNew Nothing
    labelSetMarkup label (" <b>Updating...</b>")

    boxPackStart box label PackNatural 0

    void $ label `on` realize $ do
        void $ inputAdd inFd [IOIn,IOHup,IOInvalid] priorityDefaultIdle $ do
            r <- B.createAndTrim 8192 $ \ptr -> fromIntegral <$> fdReadBuf (fromIntegral inFd) ptr 8192
            labelSetMarkup label (" <b>" ++ escape (toString r) ++ "</b>")
            return True
        return ()
    return box
  where escape []       = []
        escape ('<':xs) = '&':'l':'t':';':escape xs
        escape (x:xs)   = x:escape xs
               
batteryNew = do
    (bar,v) <- verticalBarNew defaultBatteryConfig
    void $ bar `on` realize $ do
        -- don't update until bar bootstrapped.
        _ <- flip timeoutAdd 500 $ do
            doUpdate v
            _ <- timeoutAdd (doUpdate v >> return True) (10*1000)
            return False
        return ()
    return bar
  where defaultBatteryConfig = defaultBarConfig colorFunc
          where colorFunc pct
                  | pct < 0.1 = (1, 0, 0)
                  | pct < 0.3 = (0.5, 0.5, 0)
                  | pct < 0.6 = (0.7, 0.7, 0)
                  | pct < 0.9 = (0, 1, 0)
                  | otherwise = (0, 0, 1)
        doUpdate v = do
            batNames <- enumerateBatteries
            batInfos <- mapM getBatteryInfo batNames
            when debug $ putStrLn $ show batInfos
            case partitionEithers $ catMaybes $ batInfos of
                ([], [])    -> verticalBarSetTooltip v "no battery detected" >> verticalBarSetPercent v 0.0
                ([], bats)  -> verticalBarSetTooltip v (setTooltip bats) >> verticalBarSetPercent v (head bats)
                (_:_, bats) -> verticalBarSetTooltip v (setACTooltip bats) >> verticalBarSetPercent v 1.0
        setACTooltip bats = "on AC\n" ++ setTooltip bats
        setTooltip bats = concatMap (\b -> "battery: <b>" ++ show (percent b) ++ "%</b>") bats

        -- take a number between 0 and 1 and make a number between 0 and 100%
        percent :: Double -> Double
        percent f = fromIntegral (round (f * 1000) :: Int) / 10

        enumerateBatteries = filter (not . flip elem [".",".."]) `fmap` getDirectoryContents powerSupplyDir
        -- very simplistic and inefficient way to get NOW / FULL
        getBatteryInfo bat = do
            kvs <- (foldl parseAcc [] . lines) `fmap` readFile (powerSupplyDir </> bat </> "uevent")
            case lookup "ONLINE" kvs of
                Just "1" -> return $ Just $ Left ()
                _        -> case (lookup "CHARGE_FULL" kvs, lookup "CHARGE_NOW" kvs) of
                                (Just f, Just n) -> return $ Just $ Right (fromIntegral (read n :: Int) / fromIntegral (read f :: Int))
                                _                -> return Nothing
        parseAcc acc s =
            case break (== '=') s of
                (name,'=':val) -> (drop powerSupplyPrefixLen name, val) : acc
                _              -> acc
        powerSupplyPrefixLen = 13
        powerSupplyDir = "/sys/class/power_supply"

main = do
    -- get from user
    let onScreen      = 0
        onMonitor     = 0
        barHeight     = 25
        widgetSpacing = 10

    -- hardcode the RC.
    rcParseString $ unlines
        [ "style \"default\" {"
        , "  bg[NORMAL] = \"#000000\""
        , "  fg[NORMAL] = \"#ffffff\""
        , "text[NORMAL] = \"#ffffff\""
        , "}"
        , "widget \"topbar*\" style \"default\""
        ]

    _    <- initGUI

    Just disp <- displayGetDefault
    nScreens  <- displayGetNScreens disp
    screen    <- displayGetScreen disp onScreen
    nMonitors <- screenGetNMonitors screen
    monitorSize <- screenGetMonitorGeometry screen onMonitor

    when debug $ do
        putStrLn ("screens : " ++ show nScreens)
        putStrLn ("monitors: " ++ show nMonitors)
        putStrLn ("monitor: "  ++ show monitorSize)

    let Rectangle x y w _ = monitorSize

    window <- windowNew
    widgetSetName window "topbar"
    windowSetTypeHint window WindowTypeHintDock
    windowSetScreen window screen
    windowSetDefaultSize window w barHeight
    windowMove window x y
    void $ window `on` realize $ setStrutProperties window $ strutProperties barHeight monitorSize
    
    box <- hBoxNew False widgetSpacing
    void $ box `on` sizeRequest $ return (Requisition w barHeight)
    containerAdd window box

    systray <- systrayNew screen
    clock   <- clockNew
    battery <- batteryNew

    xmonadLog <- xmonadLogNew (fromIntegral stdInput)
    boxPackStart box xmonadLog PackGrow 0

    mapM_ (\widget -> boxPackEnd box widget PackNatural 0) [toWidget systray,toWidget clock,battery]

    void $ screen `on` screenMonitorsChanged $ do
        newMonitorSize <- screenGetMonitorGeometry screen onMonitor
        let Rectangle _ _ nw nh = newMonitorSize
        putStrLn ("monitor changed: " ++ show nw ++ "x" ++ show nh)
        windowResize window nw barHeight

    widgetShowAll window

    mainGUI
