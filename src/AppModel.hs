{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AppModel where

import Control.Lens
import Monomer
import Data.Text (Text)

import qualified Monomer.Lens as L

newtype SenderNameField = SenderNameField {
  _name :: Text
  } deriving (Show, Eq)

data AppModel = AppModel {
  _expense :: Double,
  _revenue :: Double,
  _senderNameField :: SenderNameField
} deriving Show

makeLenses 'SenderNameField
makeLenses 'AppModel

instance Eq AppModel where
  (==) a b =
    a ^. expense == b ^. expense &&
    a ^. revenue == b ^. revenue &&
    a ^. senderNameField == b ^. senderNameField

  (/=) a b =
    a ^. expense /= b ^. expense &&
    a ^. revenue /= b ^. revenue &&
    a ^. senderNameField /= b ^. senderNameField
