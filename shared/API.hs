module API where

data Request = RequestPage Page

data Page = Home
          | Tournaments
          | Results
          | Rankings
          | UserHome
          deriving (Show, Eq, Ord)
