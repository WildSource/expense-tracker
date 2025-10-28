{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Monomer
import Control.Lens
import Data.Maybe
import TextShow
import Types
import AppModel

import qualified Monomer.Lens as L

data AppEvent
  = AppInit
  | CreateExpense
  | CreateRevenue
  deriving Show

type Exp = Double
type Rev = Double

buildDashboard :: (Exp, Rev) -> WidgetNode AppModel AppEvent
buildDashboard (exp, rev) =
  vstack [ label $ "Profits: " <> showt (rev - exp) <> "$"
         , spacer
         , label $ "Expenses: " <> showt exp <> "$"
         , spacer
         , label $ "Revenues: " <> showt rev <> "$"
         ]

buildButtonMenu :: TransactionMaker -> WidgetNode AppModel AppEvent
buildButtonMenu None =
  vstack [ button "Create Expense" CreateExpense
         , spacer 
         , button "Create Revenu" CreateRevenue
         ]
buildButtonMenu _  = spacer

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model =
  let exp = model ^. expense
      rev = model ^. revenue
      tm = model ^. transactionMaker
      
  in  vstack [ label "Expense Tracker"
             , spacer
             , buildDashboard (exp, rev)
             , spacer
             , buildButtonMenu tm
             , spacer
             ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  CreateExpense -> []
  CreateRevenue -> []

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
    
