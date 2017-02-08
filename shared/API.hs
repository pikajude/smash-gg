{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API where

import Data.Serialize
import Data.Serialize.Text ()
import Data.Text
import GHC.Generics
import Web.Routes.PathInfo
import Web.Routes.TH

data Request = RequestPage Page

data Page = Home
          | Tournaments
          | Results
          | Rankings
          | UserHome

          | Attendees Text
          | Events Text
          | Tournament Text
        deriving (Show, Eq, Ord, Generic)

derivePathInfo' (\ x -> if x == "Home" then "" else standard x) ''Page

data Tournament = T
                { preview   :: Text
                , date      :: Text
                , location  :: Maybe Text
                , events    :: [Text]
                , attendees :: Integer
                , slug      :: Text
                , name      :: Text
                , started   :: Bool
                } deriving (Generic)

instance Serialize Page
instance Serialize Tournament

g4 = T { preview = "https://img.youtube.com/vi/-EBtG37v60I/mqdefault.jpg"
       , date = "January 20-22 2017"
       , location = Just "CA (NorCal)"
       , events = [ "Wii U Singles", "Wii U Doubles", "World Crews: Smash 4 Wii U"
                  , "Smash 64 Singles", "Smash 64 Doubles", "Street Fighter V Singles"
                  , "Rivals of Aether Singles", "Towerfall Singles", "Catherine Singles"
                  , "Smash Draft", "Melee Singles", "Red Bull Ladder", "Melee Doubles"
                  , "Evening Ladder" ]
       , attendees = 3576
       , slug = "genesis-4"
       , name = "Genesis 4"
       , started = True
       }

beast7 = T { preview = "https://img.youtube.com/vi/AUEBdKaikMo/mqdefault.jpg"
           , date = "February 17-19 2017"
           , location = Just "Sweden"
           , events = [ "Smash 64 Singles", "Wii U Singles", "Wii U Doubles"
                      , "Melee Doubles", "Melee Singles", "Street Fighter V Singles" ]
           , attendees = 635
           , slug = "beast-7"
           , name = "BEAST 7"
           , started = False
           }
