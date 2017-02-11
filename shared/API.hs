{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module API where

import Data.Serialize
import Data.Serialize.Text ()
import Data.Text
import Database.Persist.TH
import GHC.Generics
import Web.Routes.PathInfo
import Web.Routes.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tournament
    preview Text
    date Text
    location Text Maybe
    attendees Int
    slug Text
    name Text
    started Bool
    deriving Generic Show

Event
    name Text
    deriving Generic Show

TournamentEvent
    tournamentId TournamentId
    eventId EventId
    UniqueTournamentEvent tournamentId eventId
|]

data Request = Home
             | Tournaments
             | Results
             | Rankings
             | UserHome

             | Attendees Text
             | Events Text
             | T Text
             deriving (Show, Generic)

data Response = HomeR [(Tournament, [Event])]
              | TournamentsR
              | ResultsR
              | RankingsR
              | UserHomeR

              | AttendeesR
              | EventsR
              | TournamentR
              deriving Generic

derivePathInfo' (\ x -> if x == "Home" then "" else standard x) ''Request

instance Serialize Tournament
instance Serialize Event
instance Serialize Request
instance Serialize Response

g4 = Tournament { tournamentPreview = "https://img.youtube.com/vi/-EBtG37v60I/mqdefault.jpg"
                , tournamentDate = "January 20-22 2017"
                , tournamentLocation = Just "CA (NorCal)"
                , tournamentAttendees = 3576
                , tournamentSlug = "genesis-4"
                , tournamentName = "Genesis 4"
                , tournamentStarted = True
                }

beast7 = Tournament { tournamentPreview = "https://img.youtube.com/vi/AUEBdKaikMo/mqdefault.jpg"
                    , tournamentDate = "February 17-19 2017"
                    , tournamentLocation = Just "Sweden"
                    , tournamentAttendees = 635
                    , tournamentSlug = "beast-7"
                    , tournamentName = "BEAST 7"
                    , tournamentStarted = False
                    }
