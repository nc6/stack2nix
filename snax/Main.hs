{-# LANGUAGE OverloadedStrings  #-}

import Options.Generic (unwrapRecord)
import Snax (run)

main :: IO ()
main = do
  x <- unwrapRecord "Snax"
  run x

