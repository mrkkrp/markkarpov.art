{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Exhibition.PerYear
  ( ExhibitionPerYear (..),
  )
where

import Data.Aeson
import Data.Time
import Exhibition (Exhibition)
import Route (WithPage)

-- | Exhibitions grouped by year.
data ExhibitionPerYear = ExhibitionPerYear
  { epyYear :: Year,
    epyExhibitions :: [WithPage Exhibition]
  }

instance ToJSON ExhibitionPerYear where
  toJSON ExhibitionPerYear {..} =
    object
      [ "year" .= epyYear,
        "exhibition" .= epyExhibitions
      ]
