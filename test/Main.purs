module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import FDY (findDoomDay, getCenturyCode, getMeDay, (%))

main :: Effect Unit
main = do
  log $ show $ findDoomDay (getCenturyCode 2018) (2018%100)
  log $ show $ getMeDay 19 12 2018  
  log $ show $ getMeDay 29 1 1996
  log "You should add some tests."
