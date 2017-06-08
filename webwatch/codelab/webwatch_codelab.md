#### ZuriHac Beginner Track Exercise

**Description:** A command-line tool which watches a certain webpage for links
with keywords (e.g. search http://news.ycombinator.com for "Haskell"). When
such a link is found, it sends a message to the [Slack](https://slack.com/)
channel.

**Learning goals:**
* How to write a basic command-line application
* How to load configuration from a config-file
* How to fetch and parse a HTML page
* How to write HTTP client for REST API
* How to use the Reader/State monad transformers

Let's start!


## Development environment

The only tool you need to build this project is
[Stack](http://haskellstack.org/) – The Haskell Tool Stack. Follow the install
instructions on its homepage.

## New project

Create a new project named `webwatch`, derived from the `simple` template:

```
stack new --resolver=lts-8.17 webwatch simple
cd webwatch
```

We got an application skeleton! See what it contains:

```
src/Main.hs    -- Application entry point
LICENSE
webwatch.cabal -- Build system config
Setup.hs       -- Haskell package installer
stack.yaml     -- Modern Haskell build tool config
```

The next step is to initialize the environment. We need to set up the sandbox that
we will use for development:

```
stack setup
```

This might take a while. Stack is downloading all required libraries to make an
isolated development sandbox (so that we avoid collisions with libraries in different versions
that are installed in the system). After it is completed, try to build and run the code to get
a bit more familiar with Stack:

```
stack build
stack exec webwatch
```

It will print:

```
hello world
```

A few more useful Stack commands:
* Load your module inside of Haskell REPL: `stack ghci`
* Run tests: `stack test`

Pretty awesome! For self-study, see the following guide:
http://docs.haskellstack.org/en/stable/GUIDE/. To learn more about Cabal and
Haskell package installer, start here:
https://www.haskell.org/cabal/users-guide/.

## Loading configuration from a file

Webwatch is a simple commandline application that runs in the foreground. It runs
forever, until you kill it (ie. press the CTRL-C key).

Because we don't want to recompile the whole application everytime we want to change
the patterns to watch for, we load them from a configuration file. The `configurator`
library provides functions to load configuration options from a file.

Let's first define the `Config` object and the corresponding parser in the
`app/Main.hs` file. Replace the contents of the file with the following code:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure)

data Config = Config
    { cPatterns        :: [T.Text]
    , cUrl             :: !T.Text
    , cInterval        :: !Int
    , cSlackWebhookUrl :: !T.Text
    } deriving (Show)

parseConfig :: C.Config -> IO Config
parseConfig conf = do
    cPatterns        <- C.require conf "patterns"
    cUrl             <- C.require conf "url"
    cInterval        <- C.require conf "interval"
    cSlackWebhookUrl <- C.require conf "slack.webhook_url"
    return Config {..}


main :: IO ()
main = do
    args <- getArgs
    case args of
        [confPath] -> do
            config <- parseConfig =<< C.load [C.Required confPath]
            putStrLn $ "Got a config file:"
            putStrLn $ show config
        _ -> do
            putStrLn "Usage: webwatch CONFIG"
            exitFailure
```

Now try to build the project:

`stack build`

You'll see a failure because the file now depends two libraries, `text` and `configurator`,
which we need to mention in the cabal file:

```
$ stack build

[1 of 1] Compiling Main

…/webwatch/src/Main.hs:8:1: error:
    Failed to load interface for ‘Data.Text’
    It is a member of the hidden package ‘text-1.2.2.1’.
    Perhaps you need to add ‘text’ to the build-depends in your .cabal file.
    Use -v to see a list of the files searched for.

    Process exited with code: ExitFailure 1
```

Open the `webwatch.cabal` file and add `text` and `configurator` to the `build-depends` list.

```
executable webwatch
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                     , text
                     , configurator
```

Now we can build and run the webwatch binary:

```
$ stack build
…
$ stack exec webwatch
Usage: webwatch CONFIG
```

We need to give it one argument – the config file to load. Let's create `webwatch.conf`
and fill it with some sensible thinks to watch.

```
url = "http://news.ycombinator.com/"
patterns = ["google", "haskell", "microsoft", "apple"]
interval = 5
slack {
    webhook_url = "https://hooks.slack.com/services/TTTTTTTTT/CCCCCCCCC/XXXXXXXXXXXXXXXXXXXXXXXX"
}
```

Now we can see if we can load this file:

```
$ stack exec webwatch -- webwatch.conf
Got a config file:
Config {cPatterns = ["google","haskell","microsoft","apple"], cUrl = "http://news.ycombinator.com/", cInterval = 5, cSlackWebhookUrl = "https://hooks.slack.com/services/TTTTTTTTT/CCCCCCCCC/XXXXXXXXXXXXXXXXXXXXXXXX"}
```

Contact us at the Hackathon to obtain the real "secret" `hooks.slack.com/...`
URL for `zurihac2017.slack.com`.

## Keeping state across checks

We need to remember which links we've already seen and which ones are new, so that we
don't send the same link multiple times (we're not building a spam-bot, after all!).

Our state is simply a set of links which we've already seen, implemented using the `HashSet`
data structure from the `unordered-containers` package.

We make use of the `ReaderT` and `StateT` monad transformers to give us access to the
configuration (`ReaderT`) and manage the set of links we've seen so far (`StateT`).

The function `slog` is just for convenience so that we can log a string to stderr from
within a `WebWatchM` context.

```haskell
import qualified Data.HashSet as HS
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.State     (StateT, evalStateT, state)
import           Control.Monad.Trans     (liftIO)
import           System.IO               (hPutStrLn, stderr)

…

type LinkSet = HS.HashSet T.Text

type WebWatchM = ReaderT Config (StateT LinkSet IO)

slog :: String -> WebWatchM ()
slog msg = liftIO $ hPutStrLn stderr msg
```

This new code depends on some additional packages which we need to add to
the cabal file: `unordered-containers` and `mtl`.

```
  build-depends:       base >= 4.7 && < 5
                     , text
                     , configurator
                     , mtl
                     , unordered-containers
```

Given a `LinkSet` of already seen links, and a list of links, we need to be
able to tell which ones are new. For convenience we also return the new
`LinkSet`.

```haskell
addLinks :: [T.Text] -> LinkSet -> ([T.Text], LinkSet)
addLinks links set =
    (new, HS.union set (HS.fromList new))
  where
    new = filter (\l -> not $ l `HS.member` set) links
```

You can test this functions if you run `stack ghci` (the first line
is required so that `GHCi` automatically converts strings into `Text`):

```
$ stack ghci
*Main> :set -XOverloadedStrings
*Main> addLinks ["https://zurihac.info"] Data.HashSet.empty
(["https://zurihac.info"],fromList ["https://zurihac.info"])
*Main>
```

*Note*: `fromList …` is how Haskell pretty-prints a `HashSet` – as a list of elements that are in it.

And if we try to add a link which is already in the set:

```
*Main> let (_, set) = addLinks ["https://zurihac.info"] Data.HashSet.empty
*Main> addLinks ["https://zurihac.info"] set
([],fromList ["https://zurihac.info"])
*Main>
```

You can exit ghci by pressing CTRL-D or writing `:q` followed by the enter key.

## The main watch function

We write the main watch function such that it fetches the links, sends the
message, then sleeps for some time and then it is done. It runs inside our
`WebWatchM` monad, which gives it (read-only) access to the `Config` object and
the ability to update the `LinkSet`.

The function is aptly named `watchOnce` because it runs the checks once
and then is done.

```haskell
import           Control.Concurrent      (threadDelay)

…

watchOnce :: WebWatchM ()
watchOnce = do
    Config {..} <- ask
    slog $ "Getting links from " ++ T.unpack cUrl

    -- TODO: Fetch the links

    -- TODO: Send message

    slog $ "Sleeping " ++ show cInterval ++ " minute(s)"
    liftIO $ threadDelay (cInterval * 60 * 1000 * 1000)
```


Because we want to run these checks repeatedly, we write a second
function which uses `forever` to repeatedly call the `watchOnce`
function until the world ends. When working with monad transformer
stacks, you usually have one function which runs the whole stack –
in our case `ReaderT` + `StateT`.

```haskell
import           Control.Monad           (forever, unless)

…

webWatch :: Config -> IO ()
webWatch config =
    evalStateT (runReaderT (forever watchOnce) config) HS.empty
```

And we can hook it up into our `main` function, where instead of printing
the configuration, we run the `webWatch` function:


```haskell
main = …

            -- replace this
            putStrLn $ "Got a config file:"
            putStrLn $ show confi

            -- with that
            webWatch config

```

Now when you build and run it you should see that it wants to check the links
and then sleep for 5 minutes:

```
$ stack build
$ stack exec webwatch -- webwatch.conf
Getting links from http://news.ycombinator.com/
Sleeping 5 minute(s)
```

We don't want to wait five minutes so kill it with CTRL-C.

For testing it might be worth decreasing the inverval to a few seconds. The
config file only allows you to specify multiples of one minute, if you want
to go lower than that you'll have to hardcode it in the source code. Remember
though, `threadDelay` is specified in microseconds, so to sleep for 5 seconds
you need to use `threadDelay (5 * 1000 * 1000)`.


Now that we have the main loop ready, there are two parts missing:

 - fetching the links from the webpage
 - sending the message

Let's tackle the first one.

## Fetching links

The main file is getting quite big, so we want to move all the functions which deal with fetching the links into a separate module:

Create the file `src/WebWatch/GetLinks.hs`:

```haskell
-- | This module is responsible for scraping for links on a page containing
-- certain keywords.
{-# LANGUAGE OverloadedStrings #-}
module WebWatch.GetLinks
    ( Link (..)
    , getMatchingLinks
    ) where

import qualified Data.Text               as T

data Link = Link
    { lTitle :: !T.Text
    , lHref  :: !T.Text
    } deriving (Show)

getMatchingLinks
    :: [T.Text]  -- ^ Patterns to watch out for.
    -> T.Text    -- ^ The URL of the HTML page we want to check.
    -> IO [Link] -- ^ All links within that HTML page.
getMatchingLinks patterns uri = do
    return []
```

and register it in the cabal file:

```
Executable webwatch
  …

  Other-modules:
    WebWatch.GetLinks
```

Now we can make use of this function in `watchOnce`. To check which links
we've collected from the webpage we can use the `slog` helper function.

```haskell
watchOnce = do
    …

    -- TODO: Fetch the links
    links <- liftIO $ getMatchingLinks cPatterns cUrl
    slog $ "All links: " ++ show links
```

But this will always print an empty list, because we're not actually doing
anything useful in `getMatchingLinks`. Let's fix that.

The `http-conduit` library makes it really easy to fetch webpages.
`network-uri` provides functions to deal with URIs. And `tagsoup` is used
to convert the HTML page into a list of structured tags. Don't forget to add
those packages to the cabal file!


```haskell
import           Data.Maybe              (mapMaybe)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.HTTP.Conduit    as Http (parseUrl)
import qualified Network.HTTP.Simple     as Http
import qualified Network.URI             as Uri
import qualified Text.HTML.TagSoup       as TagSoup

getMatchingLinks
    :: [T.Text]  -- ^ Patterns to watch out for.
    -> T.Text    -- ^ The URL of the HTML page we want to check.
    -> IO [Link] -- ^ All links within that HTML page
getMatchingLinks patterns uri = do
    req <- Http.parseUrl (T.unpack uri)
    lbs <- Http.getResponseBody <$> Http.httpLbs req

    -- Convert the 'ByteString' into 'Text'
    let htmlPageAsText = TL.toStrict $ TL.decodeUtf8 lbs

    return $ extractLinks htmlPageAsText

extractLinks :: T.Text -> [Link]
extractLinks = findLinks . TagSoup.parseTags
  where
    findLinks (TagSoup.TagOpen "a" args : tags0) =
        let closeLink = (== TagSoup.TagClose "a") in
        case (lookup "href" args, break closeLink tags0) of
            (Nothing,   (_,     tags1))       -> findLinks tags1
            (_,         (_,     []))          -> []
            (Just href, (title, (_ : tags1))) ->
                Link (TagSoup.innerText title) href : findLinks tags1

    findLinks (_ : tags0) = findLinks tags0
    findLinks [] = []
```

So far we've extracted all links from the webpage, but we only want
those which match one of the patterns. That's a simple matter of filtering
the list. To make the code more robust we then normalize the URLs and make
them absolute.

```haskell
makeAbsolute :: T.Text -> [Link] -> [Link]
makeAbsolute url = mapMaybe $ \l -> do
    base <- baseUri
    href <- Uri.parseURI $ T.unpack (lHref l)
    return l {lHref = T.pack $ show $ href `Uri.relativeTo` base}
  where
    baseUri = Uri.parseURI $ T.unpack url

matchingLinks :: [T.Text] -> [Link] -> [Link]
matchingLinks patterns = filter $
    \l -> any (`T.isInfixOf` T.toLower (lTitle l)) lpatterns
  where
    lpatterns = map T.toLower patterns

…

getMatchingLinks patterns uri = do
    …

    -- old:
    return $ extractLinks htmlPageAsText
    -- new:
    return $
        makeAbsolute uri $
        matchingLinks patterns $
        extractLinks htmlPageAsText
```

That concludes the code in the GetLinks module. We can run webwatch to
see which links it has found:

```
$ stack build
$ stack exec webwatch -- webwatch.conf
Getting links from http://news.ycombinator.com/
[Link {lTitle = "Amazon reportedly working on proper Android \8216Ice\8217 smartphones with Google\8217s apps", lHref = "https://www.theverge.com/circuitbreaker/2017/6/5/15739540/amazon-android-ice-smartphones-google-apps-services-fire-phone-report"}]
Sleeping 5 minute(s)
```

Only one link, oh well… but it works!

## Matching new links against the LinkSet to find out which ones are new

Now that we have the links, we need to match them against those in the
`LinkSet` and find out which ones are new. We've already written that function
before, we just need to use it on the `LinkSet` that is managed by the `StateT`
monad transformer. The `Control.Monad.State` module proides a neat function for
just that:

```haskell
watchOnce = do
    …

    links <- liftIO $ getMatchingLinks cPatterns cUrl
    slog $ "All links: " ++ show links

    newLinks <- state (addLinks links)
    slog $ "New links: " ++ show newLinks
```

If you try to build this you'll see that it fails:

```
$ stack build
[2 of 2] Compiling Main

…/webwatch/src/Main.hs:60:33: error:
    • Couldn't match type ‘Link’ with ‘T.Text’
      Expected type: [T.Text]
        Actual type: [Link]
    • In the first argument of ‘addLinks’, namely ‘links’
      In the first argument of ‘state’, namely ‘(addLinks links)’
      In a stmt of a 'do' block: newLinks <- state (addLinks links)

    Process exited with code: ExitFailure 1
```

Somehow the type of the first argument of `addLinks` doesn't match what
it should be. And indeed, `links` (which is what's returned by `getMatchingLinks`)
has the type `[Link]` but the `addLinks` function expects `[T.Text]`.

We defined the `addLinks` function in that shape because we didn't have a `Link`
type yet. But it's easy to rewrite it such that it operates on `Link` instead of
`T.Text`:


```haskell
addLinks :: [Link] -> LinkSet -> ([Link], LinkSet)
addLinks links set =
    (new, HS.union set (HS.fromList $ map lHref new))
  where
    new = filter (\l -> not $ lHref l `HS.member` set) links
```

To test that updating the `LinkSet` works across multiple iterations of the
`watchOnce` function, let's decrease the sleep time to to seconds:

```
watchOnce = do
    …
    liftIO $ threadDelay (2 * 1000 * 1000)
```

```
$ stack build
$ stack exec webwatch -- webwatch.conf
Getting links from http://news.ycombinator.com/
All links: [Link {lTitle = "Apple, Amazon to back Foxconn on Toshiba chip bid, Gou says", lHref = "http://asia.nikkei.com/Business/Deals/Apple-Amazon-to-back-Foxconn-on-Toshiba-chip-bid-Gou-says"}]
New links: [Link {lTitle = "Apple, Amazon to back Foxconn on Toshiba chip bid, Gou says", lHref = "http://asia.nikkei.com/Business/Deals/Apple-Amazon-to-back-Foxconn-on-Toshiba-chip-bid-Gou-says"}]

Getting links from http://news.ycombinator.com/
All links: [Link {lTitle = "Apple, Amazon to back Foxconn on Toshiba chip bid, Gou says", lHref = "http://asia.nikkei.com/Business/Deals/Apple-Amazon-to-back-Foxconn-on-Toshiba-chip-bid-Gou-says"}]
New links: []

Getting links from http://news.ycombinator.com/
All links: [Link {lTitle = "Apple, Amazon to back Foxconn on Toshiba chip bid, Gou says", lHref = "http://asia.nikkei.com/Business/Deals/Apple-Amazon-to-back-Foxconn-on-Toshiba-chip-bid-Gou-says"}]
New links: []
```

That looks good, the list of all links remains the same, and the new link is considered
*new* only in the first iteration, because it's subsequently added to the `LinkSet`
and thus ignored in the later iterations.


## Sending the messages

Now that we have found the interesting articles, we should send them to the
[Slack](https://slack.com) channel. We can do that through REST API provided by
Slack. Consuming REST API means sending HTTP POST request. For that we'll use
`http-client` and `http-client-tls` packages. Add them to the project's cabal
file.

More information on the part of the Slack API we will use can be found
[here](https://api.slack.com/incoming-webhooks).

Create the file `src/WebWatch/Slack.hs`:

```haskell
-- | This module contains code for sending messages to the Slack channel.
{-# LANGUAGE OverloadedStrings #-}
module WebWatch.Slack
    ( sendLinks
    ) where

import qualified Data.Text               as T
import           WebWatch.GetLinks

sendLinks
    :: T.Text -- ^ Special access URL
    -> [Link] -- ^ Links
    -> IO ()
sendLinks webhookUrl links = do
    return ()
```

and register it in the cabal file:

```
Executable webwatch
  …

  Other-modules:
    WebWatch.GetLinks
    WebWatch.Slack
```

Now you have all that's needed to finish the `watchOnce` function:

```haskell
watchOnce = do
    …

    unless (null newLinks) $ do
        slog $ "Sending slack message..."
        catchExceptions () $ Slack.sendLinks cSlackWebhookUrl newLinks
```

We still have to finish the Slack client. To send data to the service, we'll
use JSON and `aeson` library (remember to update cabal dependencies). Open
`src/WebWatch/Slack.hs` and add: `Payload` data (message format for
communication), its Aeson wrapper for automatic JSON serialization and
deserialization, and the constructor `mkPayload`:

```haskell
import           Data.Aeson              ((.=))
import qualified Data.Aeson              as Aeson
import           Data.Monoid             ((<>))

data Payload = Payload
    { payloadText :: !T.Text
    } deriving (Show)

instance Aeson.ToJSON Payload where
    toJSON p = Aeson.object
        [ "text" .= payloadText p
        ]

mkPayload
    :: [Link]
    -> Payload
mkPayload links = Payload
    { payloadText = T.unlines $
        [ "We found a few links matching your query."
        , ""
        ] ++
        [ "- " <> lTitle link <> "\n  " <> lHref link
        | link <- links
        ]
    }
```

The core part of Stack client is `sendLinks` method. It creates the payload out
of the given links, build the HTTP POST requests and sends it to the Slack
service endpoint. Let's write it:

```haskell
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http

sendLinks
    :: T.Text -- ^ Special access URL
    -> [Link] -- ^ Links
    -> IO ()
sendLinks webhookUrl links = do
    -- Create payload
    let payload = mkPayload links

    -- Initialize an HTTP manager
    manager <- Http.newManager Http.tlsManagerSettings

    -- We create an initial request by parsing the URL
    request0 <- Http.parseRequest (T.unpack webhookUrl)

    -- But we need to set a few more details:
    let request = request0
            { Http.method      = "POST"
            , Http.requestBody = Http.RequestBodyLBS (Aeson.encode payload)
            }

    -- We perform the request (but we can ignore the result).
    _ <- Http.httpLbs request manager

    return ()
```

That's all!
