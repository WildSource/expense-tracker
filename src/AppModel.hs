{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AppModel where

import Control.Lens
import Monomer
import Data.Text (Text)

import qualified Monomer.Lens as L

data TransactionMaker
  = MakeExp
  | MakeRev
  | MakeSub
  | None
  deriving Show

data AppModel = AppModel {
  _expense :: Double,
  _revenue :: Double,
  _transactionMaker :: TransactionMaker
} deriving (Show, Eq)

makeLenses 'AppModel

