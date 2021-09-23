module Brightness (setBrightness, incBrightness, decBrightness) where

import XMonad.Core
import Control.Monad.Reader (asks)
import Control.Monad (forM_, when)
import Graphics.X11.Xrandr
import Graphics.X11


getBacklightAtom :: X Atom
getBacklightAtom = getAtom "Backlight"

getBrightness' :: RROutput -> X (Maybe Int)
getBrightness' output = withDisplay $ \dpy -> do
    atom <- getBacklightAtom
    a <- io $ xrrGetOutputProperty dpy output atom 0 4 False False 0
    pure $ case a of
        Just (19, 32, 0, d) -> Just $ fromIntegral $ head d
        _ -> Nothing

getBrightness :: RROutput -> X (Maybe Double)
getBrightness out = do
    mm <- queryMinMax out
    b <- getBrightness' out
    pure $ case (mm, b) of
        (Just (min, max), Just cur) -> Just $ brightness cur min max
        _ -> Nothing
    where
        brightness :: Int -> Int -> Int -> Double
        brightness cur min max = (fromIntegral $ cur - min) / (fromIntegral $ max - min)

setBrightness' :: RROutput -> Double -> X ()
setBrightness' rro value = withDisplay $ \dpy -> do
    mm <- queryMinMax rro
    atom <- getBacklightAtom
    case mm of
        Just (min, max) -> do
            let newValue = convertBrightness min max value
            io $ xrrChangeOutputProperty dpy rro atom 19 32 0 [fromIntegral newValue]
    where
        convertBrightness :: Int -> Int -> Double -> Int
        convertBrightness min max value = round $ value * fromIntegral (max - min)

queryMinMax :: RROutput -> X (Maybe (Int, Int))
queryMinMax out = withDisplay $ \dpy -> do
    atom <- getBacklightAtom
    info <- io $ xrrQueryOutputProperty dpy out atom
    pure $ case info of
        Just i -> do
            let value = xrr_pi_values i
            Just (fromIntegral $ head value, fromIntegral $ head $ tail value)
        _ -> Nothing

getOutputs :: X [RROutput]
getOutputs = withDisplay $ \dpy -> do
    root <- asks theRoot
    res  <- io $ xrrGetScreenResourcesCurrent dpy root
    pure $ case res of
        Just x -> xrr_sr_outputs x
        _ -> []

forOutputs :: (RROutput -> X ()) -> X ()
forOutputs f = do
    outputs <- getOutputs
    forM_ outputs f

setBrightness :: Double -> X ()
setBrightness value = forOutputs $ \o -> setBrightness' o value

incBrightness :: Double -> X ()
incBrightness value = forOutputs $ \o -> do
    b <- getBrightness o
    mm <- queryMinMax o
    case (b, mm) of
        (Just bright, Just (min, max)) -> do
            let newValue = value + bright
            setBrightness' o $ clamp newValue
    where clamp = max 0 . min 1

decBrightness :: Double -> X ()
decBrightness value = incBrightness (-value)
