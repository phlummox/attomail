
module Version

  where

import            Data.Version  (showVersion)
import qualified  Paths_attomail  as P (version)

getVersion :: String
getVersion = version
  where
    version :: String
    version = showVersion P.version





