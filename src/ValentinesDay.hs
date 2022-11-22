{-# LANGUAGE GADTs #-}
module ValentinesDay where

data Approval = Yes | No | Maybe

data Cuisine = Korean | Turkish

data Genre = Crime | Horror | Romance | Thriller 
    deriving (Eq)

data Activity where
  BoardGame :: Activity
  Chill :: Activity
  Movie :: Genre -> Activity
  Restaurant :: Cuisine -> Activity
  Walk :: Integer -> Activity

rateActivity :: Activity -> Approval
rateActivity activity = case activity of
    BoardGame -> No
    Chill -> No
    Movie g
        | g == Romance -> Yes
        | otherwise -> No
    Restaurant c -> case c of
        Korean -> Yes
        Turkish -> Maybe
    Walk d 
        | d < 3 -> Yes 
        | d <= 5 -> Maybe
        | otherwise -> No


