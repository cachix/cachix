module Cachix.Client.HumanSize where

import Protolude
import Text.Printf (printf)
import Prelude qualified

humanSize :: Double -> Text
humanSize size
  | size < unitstep 1 = render "B" size
  | size < unitstep 2 = render "KiB" $ size / unitstep 1
  | size < unitstep 3 = render "MiB" $ size / unitstep 2
  | size < unitstep 4 = render "GiB" $ size / unitstep 3
  | otherwise = render "TiB" $ size / unitstep 4
  where
    unitstep :: Int -> Double
    unitstep i = 1024.0 ^ i
    render :: Text -> Double -> Text
    render unit unitsize =
      toS (printf "%.2f" unitsize :: Prelude.String) <> " " <> unit
