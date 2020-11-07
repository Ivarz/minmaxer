module Minmaxer
    ( Volume (..)
      , VolumeSide (..)
      , Device (..)
      , Mixer (..)
      , MixerError (..)
      , someFunc
      , mkVolume
      , mkDevice
      , test_string
      , pairValues
      , parseVolume
      , parseMixer
      , readCurrentMixer
      , printCurrentMixer
      , applyMixer
      , findVolume
      , setVolume
      , clamp
    ) where

import System.Process

data Volume = Vol {left :: Int, right :: Int} deriving (Eq)
data VolumeSide = LeftV | RightV | BothV deriving (Eq, Show)

instance Show Volume where
    show v = (show (left v)) ++ ":" ++ (show (right v))

data Device = Microphone deriving (Eq)
instance Show Device where
    show Microphone = "mic"

data MixerError = NoDevices | InvalidVolume | InvalidDevice | InvalidString deriving (Eq)
instance Show MixerError where
    show NoDevices = "no devices"
    show InvalidVolume = "invalid volume value"
    show InvalidDevice = "invalid device"
    show InvalidString = "invalid mixer string"

data Mixer = Mixer {
            volumes :: [(String, Volume)]
            , recSource :: Device } deriving (Eq)

instance Show Mixer where
    show mixer = volumes_str ++ rec_dev_str
        where volumes_str = foldr (++) "" (map (\(k,v)-> k ++ " " ++ (show v) ++ " ")  (volumes mixer))
              rec_dev_str = "=rec " ++ (show (recSource mixer))

type MixerCommand = String
mkVolume :: Int -> Int -> Either MixerError Volume
mkVolume l r
     | l > 100 || r > 100 = Left InvalidVolume
     | l < 0 || r < 0 = Left InvalidVolume
     | otherwise = Right (Vol l r)

parseVolume :: String -> Either MixerError Volume
parseVolume s = let leftStr = takeWhile ((/=) ':') s
                    rightStr = drop 1 $ dropWhile ((/=) ':') s
                    left = (read leftStr) :: Int
                    right = (read rightStr) :: Int
                in mkVolume left right

--parseVolumes :: [String] -> Either MixerError [Volume]
mkDevice :: String -> Either MixerError Device
mkDevice s
    | s == "mic" = Right Microphone
    | otherwise = Left InvalidDevice

test_string :: String
test_string = "vol 68:68 pcm 100:100 speaker 59:59 mic 67:67 mix 37:37 rec 37:37 igain 0:0 ogain 100:100 =rec mic"

pairValues :: [String] -> Either MixerError [(String, String)]
pairValues [] = Right []
pairValues [x] = Left InvalidString
pairValues (x:y:zs) = (:) (x,y) <$> (pairValues zs)

lookup' :: String -> [(String, String)] -> Either MixerError String
lookup' a b = case lookup a b of
                Just val -> Right val
                Nothing -> Left InvalidString

mkMixer :: [(String, Volume)] -> Device -> Either MixerError Mixer
mkMixer [] dev = Left NoDevices
mkMixer vols dev = Right (Mixer vols dev)

parseMixer :: String -> Either MixerError Mixer
parseMixer s = do
    pairs <- pairValues (words s)
    let volstr_pairs = filter (\x -> (fst x) /= "=rec") pairs
    let volnames = map fst volstr_pairs
    let volumes_str = map snd volstr_pairs
    volumes <- mapM parseVolume volumes_str
    rec_dev_str <- lookup' "=rec" pairs
    rec_dev <- mkDevice rec_dev_str
    return rec_dev
    mkMixer (zip volnames volumes) rec_dev


readCurrentMixer :: IO (Either MixerError Mixer)
readCurrentMixer = do
    mixer_string <- readProcess "mixer" ["-s"] []
    let mixer = parseMixer mixer_string
    return mixer

printCurrentMixer :: IO ()
printCurrentMixer = do
    mixer <- readCurrentMixer
    putStrLn $ show mixer

applyMixer :: Mixer -> IO()
applyMixer mixer = callCommand ("mixer " ++ (show mixer))
--applyMixer (Left err) = putStrLn ("Error setting mixer: " ++ show err)

findVolume :: String -> Mixer -> Maybe Volume
findVolume dev mixer = lookup dev (volumes mixer)

clamp :: Int -> Int -> Int -> Int
clamp a z i = if i > z then z else if i < a then a else i

setVolume :: Mixer -> String -> VolumeSide -> Int -> Mixer
setVolume mixer dev side volume = new_mixer
                            where rest_of_volumes = filter (\x -> (fst x) /= dev) (volumes mixer)
                                  adjusted_volume = clamp 0 100 volume
                                  new_mixer = case lookup dev (volumes mixer) of
                                    Nothing -> mixer
                                    Just vol -> case side of
                                        LeftV -> Mixer ((dev, Vol adjusted_volume (right vol)) : rest_of_volumes) (recSource mixer)
                                        RightV -> Mixer ((dev, Vol (left vol) adjusted_volume) : rest_of_volumes) (recSource mixer)
                                        BothV -> Mixer ((dev, Vol adjusted_volume adjusted_volume) : rest_of_volumes) (recSource mixer)
