module Date where

data Occurence = Daily
  | Weekly
  | Monthly
  | Yearly
  deriving Show

newtype Day = Day Int
  deriving Show

mkDay :: Int -> Maybe Day
mkDay day
  | day > minDays && day < maxDays = Just $ Day day
  | otherwise = Nothing
  where
    minDays = 0
    maxDays = 32

data Month = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq)

instance Enum Month where
  toEnum 1 = January
  toEnum 2 = February
  toEnum 3 = March
  toEnum 4 = April
  toEnum 5 = May
  toEnum 6 = June
  toEnum 7 = July
  toEnum 8 = August
  toEnum 9 = September
  toEnum 10 = October
  toEnum 11 = November
  toEnum 12 = December
  toEnum _ = error "Not a valid month"
  
  fromEnum January = 1 
  fromEnum February = 2
  fromEnum March = 3
  fromEnum April = 4 
  fromEnum May = 5
  fromEnum June = 6 
  fromEnum July = 7 
  fromEnum August = 8
  fromEnum September = 9 
  fromEnum October = 10
  fromEnum November = 11
  fromEnum December = 12

data Date = Date {
  day :: Maybe Day,
  month :: Month,
  year :: Int
  } deriving Show
