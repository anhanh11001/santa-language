module Scope where

import BParser
import Test.HUnit
import Data.Either

check testName stream res = TestCase (
  assertEqual testName (parse scope "Error" stream) (Right res))
  
-- Load string from file and test

