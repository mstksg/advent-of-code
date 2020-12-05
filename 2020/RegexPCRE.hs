{-# LANGUAGE OverloadedStrings #-}

module RegexPCRE (regex) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.Foldable
import           Data.Kind
import           Data.Text (Text)
import qualified Data.Text as T
import           Foreign.C.Types
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.LookAhead
import           Text.Parser.Combinators hiding (count)
import qualified Text.Regex.PCRE.String as PCRE
import           Unsafe.Coerce

import           Util
import qualified Util.Text as T

retPartial :: PCRE.ReturnCode
retPartial = unsafeCoerce (-12 :: CInt)

execPartialHard :: PCRE.ExecOption
execPartialHard = PCRE.ExecOption 0x08000000

compile :: PCRE.ExecOption -> Text -> PCRE.Regex
compile opt pat = unsafePerformIO $ do
  result <- PCRE.compile PCRE.compAnchored opt (T.unpack pat)
  case result of
    Right re -> return re
    Left (idx, msg) -> error ("parsing regex " ++ show pat ++ ", " ++ msg)

-- Convert a regex into a Parser that parses it.  The resulting parser
-- is 'greedy with full backtracking'. It returns the longest prefix
-- of the input that matches the whole regex, or otherwise fails
-- without consuming input.
regex :: Text -> Parser Text
regex pat = root <?> T.unpack pat
  where
    re = compile execPartialHard pat
    re_final = compile PCRE.execBlank pat

    -- To match regex, we first lookAhead as far as necessary to find
    -- the longest prefix of the text that could be a match. To do
    -- this, we advance one character at a time, continuing as long as
    -- regexec returns PCRE_ERROR_PARTIAL, which indicates that there
    -- could be a longer match.

    -- When PCRE_PARTIAL_HARD is supplied, regexec always returns
    -- PCRE_ERROR_PARTIAL if there is a longer possible match, which
    -- is necessary to ensure we get the longest possible.

    -- When we've found the prefix, we match it one more time with
    -- normal options, to extract the longest actual match in that
    -- prefix, and consume and return that text.
    root = do
      txt <- lookAhead (try $ search "")
      case unsafePerformIO (PCRE.regexec re_final txt) of
         Left (code, msg) -> error msg
         Right Nothing -> mzero
         Right (Just ("", m, _, _)) -> text (T.pack m)

    search prefix = nextChar prefix <|> (eof >> return prefix)
    nextChar prefix = do
      c <- anyChar
      let prefix' = prefix <> [c]
      case unsafePerformIO (PCRE.regexec re prefix') of
        Left (code, msg) | code == retPartial -> search prefix'
        _                                     -> return prefix'


data EatResult = Full String | Partial String

regex' :: Text -> Parser Text
regex' pat = root <?> T.unpack pat
  where
    re = compile execPartialHard pat
    re_final = compile PCRE.execBlank pat
    root = do
      txt <- search ""
      case unsafePerformIO (PCRE.regexec re_final txt) of
         Right (Just ("", m, "", _)) -> return (T.pack m)
         Right _ -> mzero <?> (T.unpack pat ++ " at " ++ txt)
         Left (code, msg) -> error msg

    search prefix = nextChar prefix <|> return prefix
    nextChar prefix = do next <- try (eatChar prefix)
                         case next of
                           Partial prefix' -> search prefix'
                           Full prefix' -> return prefix'

    eatChar prefix = do
      c <- anyChar
      let prefix' = prefix <> [c]
      case unsafePerformIO (PCRE.regexec re prefix') of
        Left (code, msg) | code == retPartial -> return (Partial prefix')
        Right (Just ("", m, "", _))           -> return (Full prefix')
        Right _                               -> mzero
