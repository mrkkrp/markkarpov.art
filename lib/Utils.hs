module Utils
  ( getMatchingFiles,
    newCache',
    parseDay,
    renderDay,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Development.Shake

getMatchingFiles :: FilePattern -> Action [FilePath]
getMatchingFiles = getDirectoryFiles "" . pure

newCache' :: (() -> Action v) -> Rules (Action v)
newCache' = fmap ($ ()) . newCache

parseDay :: (MonadFail m) => Text -> m Day
parseDay = parseTimeM False defaultTimeLocale "%F" . T.unpack

renderDay :: Day -> String
renderDay = formatTime defaultTimeLocale "%d.%m.%Y"
