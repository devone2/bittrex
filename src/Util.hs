module Util where

import Data.Aeson.Types
import Data.Scientific (Scientific)

asRational :: Parser Scientific -> Parser Rational
asRational = (<$>) toRational

