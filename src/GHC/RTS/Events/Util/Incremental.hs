-- | Incremental processing of the eventlog
module GHC.RTS.Events.Util.Incremental (
    readEventLogIncremental
  ) where

import Control.Exception
import Data.ByteString.Lazy qualified as BS.Lazy
import GHC.RTS.Events
import GHC.RTS.Events.Incremental

{-------------------------------------------------------------------------------
  Read the eventlog
-------------------------------------------------------------------------------}

-- | Read the eventlog incrementally
--
-- Throws an exception if we fail to decode the header. If we fail to decode
-- any of the events, then the list will contain a pure exception. We could do
-- this more properly by introducing a "list that might terminate in an error"
-- type, but for our current purposes that's not necessary.
readEventLogIncremental :: FilePath -> IO (Header, [Event])
readEventLogIncremental fp = do
    raw <- BS.Lazy.readFile fp
    case readHeader raw of
      Left err ->
        throwIO $ FailedToDecodeHeader err
      Right (header, raw') ->
        return (header, aux $ readEvents' header raw')
  where
    aux :: [Either String Event] -> [Event]
    aux []             = []
    aux (Right e : es) = e : aux es
    aux (Left err : _) = throw $ FailedToDecodeEvent err

data InvalidEventLog =
    FailedToDecodeHeader String
  | FailedToDecodeEvent String
  deriving stock (Show)
  deriving anyclass (Exception)