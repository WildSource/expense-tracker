module AppEvent where

data AppEvent
  = AppInit
  | CreateExpense
  | CreateRevenue
  | CreateSubscription
  deriving Show
