{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import Types

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

data AppEvent
  = AppInit
  deriving (Eq, Show)

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  exp = model ^. expense
  rev = model ^. revenue
  
  widgetTree = vstack [
      label $ "Profits: " <> showt (rev - exp) <> "$",
      spacer,
      label $ "Expenses: " <> showt exp <> "$",
      spacer,
      label $ "Revenues: " <> showt rev <> "$",
      spacer,
      spacer,
      label "Sender's name: ",
      textField (senderNameField . name)
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Expense Tracker",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    senderNameField = SenderNameField { _name = "" }
    model = AppModel {
      _expense = 0,
      _revenue = 0 ,
      _senderNameField = senderNameField
      }
    
