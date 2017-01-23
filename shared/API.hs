module API where

import Data.Text

data Request = RequestPage Page

data Page = Home
          | Tournaments
          | Results
          | Rankings
          | UserHome

          | Attendees Text
          | Events Text
          | Tournament Text
          deriving (Show, Eq, Ord)

data Tournament = T
                { preview   :: Text
                , date      :: Text
                , location  :: Text
                , events    :: [Text]
                , attendees :: Integer
                , slug      :: Text
                , name      :: Text
                }

g4 = T { preview = "https://img.youtube.com/vi/-EBtG37v60I/mqdefault.jpg"
       , date = "January 20-22 2017"
       , location = "CA (NorCal)"
       , events = [ "World Crews: Smash 4 Wii U", "Towerfall Singles"
                  , "Rivals of Aether Singles", "Red Bull Ladder"
                  ]
       , attendees = 3576
       , slug = "genesis-4"
       , name = "Genesis 4"
       }
