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

regex :: Text -> Parser Text
regex pat = root <?> T.unpack pat
  where
    re = compile execPartialHard pat
    re_final = compile PCRE.execBlank pat

    root = do
      txt <- lookAhead (matching "")
      case unsafePerformIO (PCRE.regexec re_final (T.unpack txt)) of
         Left (code, msg) -> error msg
         Right Nothing -> mzero
         Right (Just ("", m, "", _)) -> text (T.pack m)

    matching start = do
      c <- anyChar
      let substr = start ++ [c]
      case unsafePerformIO (PCRE.regexec re substr) of
        Left (code, msg)
          | code == retPartial -> (partial substr)
          | otherwise -> error msg
        Right Nothing -> mzero
        Right (Just ("", m, _, _)) -> return (T.pack m)

    partial start =
      (do c <- anyChar
          let substr = start ++ [c]
          case unsafePerformIO (PCRE.regexec re substr) of
            Left (code, msg)
              | code == retPartial -> (partial substr)
              | otherwise -> error msg
            Right Nothing -> return (T.pack start)
            Right (Just ("", m, _, _)) -> return (T.pack m))
      <|> (eof >> return (T.pack start))
