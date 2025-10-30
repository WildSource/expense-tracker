{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Monomer
import Control.Lens
import Data.Maybe
import TextShow
import Types
import AppModel
import AppEvent
import UI

import qualified Monomer.Lens as L

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  CreateExpense -> [Model $ model & transactionMaker %~ (\ old -> MakeExp)]
  CreateRevenue -> [Model $ model & transactionMaker %~ (\ old -> MakeRev)]
  CreateSubscription -> [Model $ model & transactionMaker %~ (\ old -> MakeSub)]

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
      
    model = AppModel {
      _expense = 0,
      _revenue = 0 ,
      _transactionMaker = None
      }
    
