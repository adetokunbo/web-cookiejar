{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | 'Parser' for a Netscape/Mozilla cookie jar

Provides parsing functions that parse the Netscape/Mozilla cookie jar file
format, along wiht @'Builder's@ that provide an incomplete roundtrip with the
parser.

The roundtrip is incomplete because some of the fields in @Cookie@ are not saved
in the Netscape/Mozilla cookie jar; see `cookieBuilder`.
-}
module Web.Cookie.Jar
  ( -- * read/write Cookie Jar files
    writeJar
  , writeJar'
  , writeNetscapeJar
  , readJar

    -- * Cookie jar format

    -- ** parsing
  , cookieJarParser
  , cookieParser
  , parseCookieJar

    -- ** printing
  , netscapeJarBuilder
  , jarBuilder
  , jarBuilder'
  , cookieBuilder

    -- * re-exports
  , parseOnly
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
  ( Parser
  , char
  , decimal
  , endOfLine
  , isEndOfLine
  , many'
  , parseOnly
  , skipSpace
  , skipWhile
  , takeWhile1
  , try
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder
  ( Builder
  , byteString
  , char7
  , integerDec
  , toLazyByteString
  )
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime
  , utcTimeToPOSIXSeconds
  )
import Network.HTTP.Client
  ( Cookie (..)
  , CookieJar
  , createCookieJar
  , destroyCookieJar
  )


-- | Parse a @ByteString@ containing a cookie jar in the Netscape/Mozilla format
parseCookieJar :: ByteString -> Either String CookieJar
parseCookieJar = parseOnly cookieJarParser


-- | @Parser@ for a cookie jar in the Netscape/Mozilla format
cookieJarParser :: Parser CookieJar
cookieJarParser = createCookieJar <$> many' cookieParser


{- | Parser for one cookie/line in a cookie jar in the Netscape/Mozilla format
This will also consume any comment lines preceding the cookie line.

This parser recognizes the magic prefix @#HttpOnly_# and sets the appropriate
field in the @Cookie@ datatype
-}
cookieParser :: Parser Cookie
cookieParser =
  let
    httpOnlyLine = try $ "#HttpOnly_" *> cookieParser' True
    commentLine = "#" *> skipWhile notEndOfLine *> endOfLine *> cookieParser
    cookieLine = cookieParser' False
   in
    skipSpace *> (httpOnlyLine <|> commentLine <|> cookieLine)


-- | Basic parser for a line containing a cookie in the Netscape/Mozilla format
cookieParser' :: Bool -> Parser Cookie
cookieParser' cookie_http_only = do
  let
    epoch = posixSecondsToUTCTime 0
    -- component parsers
    tab = void $ char '\t'
    parseString = takeWhile1 (/= '\t')
    parseBool = True <$ "TRUE" <|> False <$ "FALSE"
    parseTime = posixSecondsToUTCTime . fromInteger <$> decimal
    parseValue = takeWhile1 notEndOfLine
  cookie_domain <- parseString
  tab
  cookie_host_only <- parseBool
  tab
  cookie_path <- parseString
  tab
  cookie_secure_only <- parseBool
  tab
  cookie_expiry_time <- parseTime
  tab
  cookie_name <- parseString
  tab
  cookie_value <- parseValue
  endOfLine <|> pure ()
  pure $
    Cookie
      { cookie_domain
      , cookie_path
      , cookie_secure_only
      , cookie_expiry_time
      , cookie_name
      , cookie_value
      , cookie_host_only
      , cookie_http_only
      , -- fields not represented by the cookie jar format
        cookie_creation_time = epoch
      , cookie_last_access_time = epoch
      , cookie_persistent = True
      }


notEndOfLine :: Char -> Bool
notEndOfLine = not . isEndOfLine . fromIntegral . ord


-- | Like 'jarBuilder' but outputs the Netscape header before the cookie lines
netscapeJarBuilder :: CookieJar -> Builder
netscapeJarBuilder = jarBuilder' netscapeHeader


netscapeHeader :: Builder
netscapeHeader = "# Netscape HTTP Cookie File\n"


-- | Print a cookie jar in the Netscape/Mozilla format, with no header
jarBuilder :: CookieJar -> Builder
jarBuilder = foldMap ((<> "\n") . cookieBuilder) . destroyCookieJar


-- | Like 'jarBuilder' but outputs a header before the cookie lines
jarBuilder' :: Builder -> CookieJar -> Builder
jarBuilder' header = (header <>) . jarBuilder


-- | Writes a cookie jar to the given path in the Netscape/Mozilla format, with no header
writeJar :: FilePath -> CookieJar -> IO ()
writeJar fp = L.writeFile fp . toLazyByteString . jarBuilder


-- | Like 'writeJar', but outputs a header before the cookie lines
writeJar' :: Builder -> FilePath -> CookieJar -> IO ()
writeJar' header fp =
  L.writeFile fp
    . toLazyByteString
    . jarBuilder'
      header


-- | Like 'writeJar', but outputs the Netscape header before the cookie lines
writeNetscapeJar :: FilePath -> CookieJar -> IO ()
writeNetscapeJar = writeJar' netscapeHeader


-- | Read a Cookie Jar from a file.
readJar :: FilePath -> IO (Either String CookieJar)
readJar = fmap parseCookieJar . B.readFile


{- | Builder for one cookie; generates a single line in the Cookie Jar file format

the values of the following fields are not output, as the file format does
support them.

- 'cookie_creation_time'
- 'cookie_last_access_time'
- 'cookie_persistent'
-}
cookieBuilder :: Cookie -> Builder
cookieBuilder c =
  let
    httpOnly True = "#HttpOnly_"
    httpOnly False = mempty
    bool True = "TRUE"
    bool False = "FALSE"
    unixTime = integerDec . round . utcTimeToPOSIXSeconds
    tab = char7 '\t'
   in
    httpOnly (cookie_http_only c)
      <> byteString (cookie_domain c)
      <> tab
      <> bool (cookie_host_only c)
      <> tab
      <> byteString (cookie_path c)
      <> tab
      <> bool (cookie_secure_only c)
      <> tab
      <> unixTime (cookie_expiry_time c)
      <> tab
      <> byteString (cookie_name c)
      <> tab
      <> byteString (cookie_value c)
