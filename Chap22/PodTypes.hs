module PodTypes where

data Podcast = Podcast
  { -- | Numeric ID for this podcast
    castId :: Integer,
    -- | Its feed URL
    castURL :: String
  }
  deriving (Eq, Show, Read)

data Episode = Episode
  { -- | Numeric ID for this episode
    epId :: Integer,
    -- | The ID of the podcast it came from
    epCast :: Podcast,
    -- | The download URL for this episode
    epURL :: String,
    -- | Whether or not we are done with this ep
    epDone :: Bool
  }
  deriving (Eq, Show, Read)