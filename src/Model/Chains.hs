module Model.Chains where

import Model.MarketModel (Market)

data ExName = Bittrex | KuCoin
  deriving (Eq, Show)

type ExMap = M.Map String Market

data Exchange = Exchange ExName ExMap
