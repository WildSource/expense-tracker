{-# LANGUAGE OverloadedStrings #-}
module Types where

import Date
import Data.Text (Text)

data Revenue = Revenu {
  sender :: Text,
  amountRev :: Double,
  dateRev :: Date
  } deriving Show

data Expense = Expense {
  recepientExp :: Text,
  amountExp :: Double,
  dateExp :: Date
  } deriving Show

data Subscription = Subscription {
  serviceName :: Text,
  amount :: Double,
  occurence :: Occurence,
  startDate :: Date,
  billingDate :: Date
  } deriving Show
