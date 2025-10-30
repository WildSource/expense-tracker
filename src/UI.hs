module UI (buildUI) where

import Monomer
import Control.Lens
import TextShow
import AppEvent
import AppModel ( AppModel(..)
                , TransactionMaker (..)
                , expense
                , revenue
                , transactionMaker
                )

import qualified Monomer.Lens as L

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

buildButtonMenu :: WidgetNode AppModel AppEvent
buildButtonMenu =
  vstack [ button "Create Expense" CreateExpense
         , spacer 
         , button "Create Revenu" CreateRevenue
         , spacer 
         , button "Create Subcription" CreateSubscription
         ]

buildTransactionForm
  :: TransactionMaker
  -> WidgetNode AppModel AppEvent
buildTransactionForm MakeExp = label "Making Expense ..."
buildTransactionForm MakeRev = label "Making Revenue ..."
buildTransactionForm MakeSub = label "Making Subscription ..."
buildTransactionForm None = spacer

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
             , buildButtonMenu
             , spacer
             , buildTransactionForm tm
             ] `styleBasic` [padding 10]
