module Parse.Location (loc) where
import Text.Parsec
import Control.Monad

loc :: Monad m => ParsecT s u m SourcePos
loc = statePos `liftM` getParserState
