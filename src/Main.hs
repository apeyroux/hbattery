{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show

import           Control.Monad
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import           Options.Generic
import           System.Directory
import           System.FilePath.Posix
import           Text.Parsec

type BattName = T.Text

data BatteryStatus = Charging | Discharging | Unknown deriving (Show, Read)

data Battery = Battery {
  battName :: T.Text
  , battCapacity :: Double
  , battStatus :: BatteryStatus
  }

instance Show Battery where
  show (Battery n c s) = show $ "Battery "
                         ++ T.unpack n
                         ++ " "
                         ++ show c
                         ++ "% "
                         ++ show s

data HBattery w = HBattery {
  verbose :: w ::: Bool <?> "Verbose mode"
  } deriving (Generic)

instance ParseRecord (HBattery Wrapped)
deriving instance Show (HBattery Unwrapped)

baseDir :: FilePath
baseDir = "/sys/class/power_supply/"

-- /sys/class/power_supply/BAT1/capacity
readCapacity :: BattName -> IO Double
readCapacity bn = do
  rf <- T.readFile (baseDir </> T.unpack bn </> "capacity")
  return $ read (T.unpack rf) :: IO Double

readStatus :: BattName -> IO BatteryStatus
readStatus bn = do
  s <- T.readFile (baseDir </> T.unpack bn </> "status")
  return $ status s
  where
    status s = read $ T.unpack (T.strip s)::BatteryStatus

lsBatt :: IO [BattName]
lsBatt = listDirectory baseDir
  >>= filterM (return . isPrefixOf "BAT")
  >>= (\t -> return $ fmap (T.pack) t)

nbBatt :: IO Int
nbBatt = fmap length lsBatt

main :: IO ()
main = do
  args <- unwrapRecord "hbattery"

  lsbatt <- lsBatt
            >>= mapM (\b -> do
                         bCapa <- readCapacity b
                         bStat <- readStatus b
                         return $ Battery b bCapa bStat
                      )
  sumBatt <- fmap sum (lsBatt >>= mapM readCapacity)
  nbbatt <- nbBatt

  when (verbose (args::HBattery Unwrapped)) $ do
    mapM_ (\b -> putStr $ T.unpack (battName b) ++ " "
                 ++ show (battCapacity b)
                 ++ "% " ++ show (battStatus b) ++ "\n") lsbatt

  putStrLn $ "Full capacity: " ++ show (sumBatt / fromIntegral nbbatt) ++ "%"
