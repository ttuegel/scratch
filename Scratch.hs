module Scratch where

import Data.Function ((&))
import Data.Maybe (fromMaybe)

optional :: Maybe Integer
optional = Nothing

handled :: Integer
handled =
  do
    n <- optional
    return n
  & fromMaybe 0
