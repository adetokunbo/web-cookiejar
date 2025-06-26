# web-cookiejar
[![GitHub CI](https://github.com/adetokunbo/web-cookiejar/actions/workflows/cabal.yml/badge.svg)](https://github.com/adetokunbo/web-cookiejar/actions)
[![Stackage Nightly](http://stackage.org/package/web-cookiejar/badge/nightly)](http://stackage.org/nightly/package/web-cookiejar)
[![Hackage][hackage-badge]][hackage]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/web-cookiejar/blob/master/LICENSE)

`web-cookiejar` provides parsing and printing functions that read and write web
cookies stored in file in the Netscape/Mozilla cookie jar file format.

Parsing and printing do not form a full data roundtrip because the cookies are
parsed into the data type [Cookie] of the [http-client] library, which has
additional fields not present in the cookie jar file format.

In addition to the parsing and printing functions, it provides utility functions
for reading and writing cookie jar files using that only depend on the haskell
base package

## Usage Example

This example demonstrates a basic use case: updating the cookie jar to reflect
any cookies in a response

```haskell
import Data.Time (getCurrentTime)
import Network.Http.Client
  ( Response
  , Request
  , Manager
  , httpLbs
  )
import Web.CookieJar
  (usingCookiesFromFile')

{- Load/save and relevant cookies when making simple request using http-client. -}
httpWithCookies :: Manager -> FilePath -> Request -> IO (Response a)
httpWithCookies manager cookieJarPath req = do
  let httpLbs' = usingCookiesFromFile' cookiePath $ flip httpLbs manager
  httpLbs' req

```

## Similar libraries

The parsing and printing in `web-cookiejar` are nearly identical to that in
[curl-cookiejar]. This library has fewer dependencies as its file-handling
utility functions only depend on functions from [System.IO].


[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/web-cookiejar.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=web-cookiejar>
[hackage-badge]:      <https://img.shields.io/hackage/v/web-cookiejar.svg>
[hackage]:            <https://hackage.haskell.org/package/web-cookiejar>
[Cookie]:             <https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Client.html#t:Cookie>
[http-client]:        <https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Client.html>
[curl-cookiejar]:     <https://hackage.haskell.org/package/curl-cookiejar>
[System.IO]:          <https://hackage.haskell.org/package/base/docs/System-IO.html>
