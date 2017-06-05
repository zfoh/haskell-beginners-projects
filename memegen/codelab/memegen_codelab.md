#### ZuriHac 2016 Beginner Track Exercise
# Web meme generator

**Description:** A web application for creating [memes](https://en.wikipedia.org/wiki/Internet_meme) from pictures:

![Printing a newline in Haskell](https://s-media-cache-ak0.pinimg.com/736x/d4/0e/34/d40e34931f55f7fb2d6e1ef7eff11b73.jpg)

**Learning goals:**
* How to use a Haskell web framework (Snap).
* How to do image processing in Haskell (using GD library).
* How to store data into a database (SQLite).
* How to build a small, real-world web application in Haskell (no HTML or CSS
  knowledge (or work) required).

**NOTE:** Instructions are tested on Ubuntu Linux 16.04, OS X. For help with
Microsoft Windows, please email ivan.kristo@gmail.com.

Let's start!


## Development environment

For this project, we will need:
* [Stack](http://haskellstack.org/) The Haskell Tool Stack
* [GD library](https://github.com/libgd/libgd) GD Graphics Library
* [SQLite](https://www.sqlite.org/) database

Steps:

1. Install Stack: http://haskellstack.org/
   * *Windows:* If you get certificate error when running Stack, open Internet
     Explorer and visit the following pages (note the *https* protocol):

     * https://www.haskell.org
     * https://www.github.com
     * https://www.stackage.org

2. Install GD library
   * On Debian-based distributions, run:

     ```sh
     sudo apt-get install libgd-dev
     ```
   * On OS X use [Homebrew](http://brew.sh/) to `brew install gd`

3. Install SQLite
   * On Debian-based distributions, run:

     ```sh
     sudo apt-get install libsqlite3-dev
     ```
   * SQLite is already pre-installed on newer versions of Mac OS X (it is called `sqlite3`). For older versions of Mac OS X, follow the [SQLite installation instructions](http://www.tutorialspoint.com/sqlite/sqlite_installation.htm).

## New project

Create a new project using Stack:

```
stack new memegen --resolver=lts-5.18
cd memegen
```

We got an application skeleton! See what it contains:

```
app/Main.hs    -- Application entry point
src/Lib.hs     -- Application logic
test/Spec.hs   -- Test suite skeleton
LICENSE
memegen.cabal  -- Build system config
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
stack exec memegen-exe
```

It will print:

```
someFunc
```

The logic behind this is contained in: *src/Lib.hs*. Check it out.

A few more useful Stack commands:
* Load your module inside of Haskell REPL: `stack ghci`
* Run tests: `stack test`

Pretty awesome! For self-study, see the following guide:
http://docs.haskellstack.org/en/stable/GUIDE/. To learn more about Cabal and
Haskell package installer, start here:
https://www.haskell.org/cabal/users-guide/.


### Auto-reloading GHCi session

There is no mature Haskell IDE yet. One thing that I (Simon) have found to
work well for console-based development is
[`ghcid`](https://github.com/ndmitchell/ghcid), which provides a convenient
auto-reloading GHCi daemon. It is set up as follows.


1. Install `ghcid` using `stack install ghcid`. This will install `ghcid` into
   the local bin path (see `stack path --local-bin`). `stack exec` will
   automatically pick up any binary that is in that folder, so that you don't
   have to edit your `PATH` environment variable.
2. Let's have a look at its help output.
   ```
   > stack exec ghcid -- --help
   Auto reloading GHCi daemon v0.6.4

   ghcid [OPTIONS] [MODULE]

   Common flags:
     -c --command=COMMAND  Command to run (defaults to ghci or cabal repl)
     -T --test=EXPR        Command to run after successful loading
     -W --warnings         Allow tests to run even with warnings
     -S --no-status        Suppress status messages
     -h --height=INT       Number of lines to use (defaults to console height)
     -w --width=INT        Number of columns to use (defaults to console width)
     -t --topmost          Set window topmost (Windows only)
     -n --notitle          Don't update the shell title/icon
        --reload=PATH      Reload when the given file or directory contents
                           change (defaults to none)
        --restart=PATH     Restart the command when the given file or directory
                           contents change (defaults to .ghci and any .cabal file)
     -C --directory=DIR    Set the current directory
     -o --outputfile=FILE  File to write the full output to
     -? --help             Display help message
     -V --version          Print version information
        --numeric-version  Print just the version number
     -v --verbose          Loud verbosity
     -q --quiet            Quiet verbosity
    ```
3. Run `ghcid` with the following flags.

   ```
   stack exec ghcid -- -c'stack ghci --main-is=memegen:memegen-exe'
   ```

   This instructs `ghcid` to start a GHCi session with all the modules
   necessary to build `memegen-exe` in the `memegen` .cabal file.

4. Insert an error in the `Lib.hs` file and observe how it is reported by
   `ghcid`.

5. You can also auto-reload the `main` function of `memegen-exe` using the
   `--test` flag of `ghcid` as follows.

   ```
   stack exec ghcid -- -c'stack ghci --main-is=memegen:memegen-exe' --test='main'
   ```

   This is super handy to auto-recompile and load your `memegen` server on
   file changes.


Running a split console-screen setup with an editor, a `ghcid` session for
immediate error messages, and a `stack ghci` session for easy REPL
interaction provides a simple, but reasonably functional IDE.


## Web application

To build the Memegen backend we will use the
[Snap framework](http://snapframework.com/).
Start by defining the *application entry point* and importing the *Snap framework*.

Create a directory *src/Memegen*. Creating a new directory will enable us to
prefix our modules. That will create a more comprehensible code overview.

1. Move *src/Lib.hs* to *src/Memegen/Lib.hs*.

2. Open *Lib.hs* and change the module definition to:

```haskell
module Memegen.Lib
    ( memegenEntry
    ) where
```

Import the Snap framework and write the entry point:

```haskell
import qualified Snap as S

memegenEntry :: IO ()
memegenEntry = S.serveSnaplet S.defaultConfig appInit

appInit :: S.SnapletInit a ()
appInit = S.makeSnaplet "memegen" "Meme generator." Nothing $ do
    S.addRoutes []
```

The initialization step sets up the application state. The main purpose of Snap
monad is state management (setting up the state, accessing and mutating it). A
state can be a database connection, access control list, backend URI, etc.  The
only state here is an empty URL mapping. That is why we have left the type
generic. Later, when we define the application state, we will be able to put a
concrete type signature.

Try to build the application by running `stack build`. It will fail.
The `snap` dependency is missing. Open the Cabal configuration and add the
library dependency:

```
library
  ...
  exposed-modules:     Memegen.Lib
  build-depends:       base >= 4.7 && < 5
                     , snap
```

To make the code compile successfully, we still need to make two changes:

* Add the *OverloadedStrings* syntax extension. It is required by `makeSnaplet`
  as it actually takes `Text` and not `String`. By enabling the
  *OverloadedStrings* syntax extension we make this conversion automatic
  (we make string literals polymorphic over the `IsString` class).
  Add `{-# LANGUAGE OverloadedStrings #-}` on top of *Lib.hs*.
* Change the *app/Main.hs* import from `Lib` to `Memegen.Lib`.
* Change *app/Main.hs* to call `memegenEntry` instead of `someFunc`.

If you got everything correctly, your *Lib.hs* will look like this:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Memegen.Lib
    ( memegenEntry
    ) where

import qualified Snap as S

memegenEntry :: IO ()
memegenEntry = S.serveSnaplet S.defaultConfig appInit

appInit :: S.SnapletInit a ()
appInit = S.makeSnaplet "memegen" "Meme generator." Nothing $ do
    S.addRoutes []
```

Build and run the application:

```
stack build
stack exec memegen-exe
```

You should get a message saying:

```
no port specified, defaulting to port 8000
Initializing memegen @ /

Listening on http://0.0.0.0:8000/
```

The web server works! Try it out by opening
[localhost:8000](http://localhost:8000) in your browser. The web page will say:

```
No handler accepted "/"
```

We haven't configured any routes. Let's fix that! Open *Lib.hs* and add:

```haskell
routes :: [(B.ByteString, S.Handler a b ())]
routes = [ ("/", S.ifTop $ S.writeBS "hello there")
         ]
```

We still have to:
* Pass the routes to `S.addRoutes`,

  ```haskell
  import qualified Data.ByteString as B

  appInit = S.makeSnaplet "memegen" "Meme generator." Nothing $ do
     S.addRoutes routes
  ```

* Add *bytestring* in the Cabal library dependency.

If you build & run the application, [localhost:8000](http://localhost:8000)
will say `hello there`.

Before we jump to more advanced topics, let's learn how to pass
arguments to request handlers. Expand the routes with `echoHandler`:

```haskell
routes = [ ("/", S.ifTop $ S.writeBS "hello there")
         , ("hello/:echoparam", S.method S.GET echoHandler)
         ]

echoHandler :: S.Handler a b ()
echoHandler = do
  Just param <- S.getParam "echoparam"
  S.writeBS $ B.append "Hello " param
```

[/hello/haskell](http://localhost:8000/hello/haskell)
will now say: `Hello haskell`!


### File upload

We have a working web application. To create a meme, we need to be able to
upload a picture. Follow the steps to add a file upload handler.

1. Extend the routes with a file upload handler mapped to `/upload`:

   ```haskell
   routes :: [(B.ByteString, S.Handler a b ())]
   routes = [ ("/", S.ifTop $ S.writeBS "hello there")
            , ("hello/:echoparam", S.method S.GET echoHandler)
            , ("upload", S.method S.POST uploadHandler)
            ]
   ```

2. Write a file upload handler function:

   ```haskell
   import qualified Snap.Util.FileUploads as S
   import qualified Data.Enumerator.List as EL
   import qualified Data.Text as T
   import           Data.Text.Encoding (decodeUtf8)
   import           Control.Monad.State (liftM, liftIO)
   import           System.FilePath ((</>))
   import           Data.Maybe (fromJust)

   uploadHandler :: S.Handler a b ()
   uploadHandler = do
     -- Files are sent as HTTP multipart form entries.
     files <- S.handleMultipart uploadPolicy $ \part -> do
       content <- liftM B.concat EL.consume
       return (part, content)
     let (imgPart, imgContent) = head files
     let fileName = fromJust (S.partFileName imgPart)

     -- Store the image in upload directory.
     -- writeFile operates inside IO monad. Snap handlers run inside Snap
     -- monad, which provides an access to IO monad. We use liftIO to
     -- execute a function inside IO monad and return to Snap monad.
     liftIO $ B.writeFile
       ("upload" </> (T.unpack $ decodeUtf8 fileName)) imgContent

     -- Show the uploaded image.
     S.writeBS imgContent

     where
       uploadPolicy :: S.UploadPolicy
       uploadPolicy =
         -- 2^24 is the maximum allowed upload file size (16MiB)
         S.setMaximumFormInputSize (2^(24::Int)) S.defaultUploadPolicy
   ```

   Add its Cabal dependencies:

   ```
   snap-core
   enumerator
   text
   mtl
   filepath
   ```

3. If the *upload* directory doesn't exist, writing the uploaded
   file will fail. We can solve this by creating the *upload* directory
   at application initialization time:

   ```haskell
   import System.Directory (createDirectoryIfMissing)

   appInit = S.makeSnaplet "memegen" "Meme generator." Nothing $ do
       S.addRoutes routes
       liftIO $ createDirectoryIfMissing True "upload"
   ```

   Don't forget to add the *directory* library dependency in Cabal config.

4. The next thing we need is an HTML file upload form. Create an empty HTML
   file anywhere in your local file system and paste the following HTML into it:

   ```html
   <html>
   <body>
     <form action="http://localhost:8000/upload" method="post" enctype="multipart/form-data">
       Top text: <input type="text" name="top" /><br />
       Select a file: <input type="file" name="image" /><br />
       Bottom text: <input type="text" name="bottom" />
       <hr />
       <input type="submit" value="Upload" />
     </form>
   </body>
   </html>
   ```

   This will be the HTML form which we will use throughout the rest
   of the project. We don't serve this file through the local http
   server but merely open the file from the local file system in
   the browser to trigger our app.

5. Build & run the application. Open the HTML file in a web browser, and
   upload an image. The image should be presented to you on the following
   screen. Also, it will be stored in *upload* directory.


### Metadata store

To be able to list created memes, we need to store their metadata in a
database. To make it simple, we will use the SQLite database. We need:
* an application state to keep the database connection,
* a record to represent the data model,
* a database schema to persist the data model,
* functions to write and read database data,
* the database initialization code.

Follow the steps:

1. Create the file *src/Memegen/App.hs* which will hold the application state:

   ```haskell
   {-# LANGUAGE FlexibleInstances #-}
   {-# LANGUAGE OverloadedStrings #-}
   {-# LANGUAGE TemplateHaskell   #-}

   module Memegen.App
       ( AppState(..)
       , db
       ) where

   import           Control.Lens (makeLenses)
   import           Control.Monad.State (get)
   import qualified Snap as S
   import qualified Snap.Snaplet.SqliteSimple as L

   data AppState = AppState
       { _db :: S.Snaplet L.Sqlite
       }

   makeLenses ''AppState

   instance L.HasSqlite (S.Handler b AppState) where
       getSqliteState = S.with db get
   ```

   Export *Memegen.App* module in Cabal config, and add *lens* and
   *snaplet-sqlite-simple* dependencies:

   ```
   library
    hs-source-dirs:      src
    exposed-modules:     Memegen.Lib
                       , Memegen.App
    build-depends:       base >= 4.7 && < 5
                       , lens
                       , snaplet-sqlite-simple
                       ...
   ```

   If you try to build the application, you will get the following error:

   ```
   While constructing the BuildPlan the following exceptions were encountered:

   --  Failure when adding dependencies:
         snaplet-sqlite-simple: needed (-any), stack configuration has no specified version (latest applicable is 0.4.8.3)
       needed for package memegen-0.1.0.0

   --  While attempting to add dependency,
       Could not find package snaplet-sqlite-simple in known packages

   Recommended action: try adding the following to your extra-deps in memegen/stack.yaml
   - snaplet-sqlite-simple-0.4.8.3

   You may also want to try the 'stack solver' command
   ```

   Stack couldn't find the module because it is not known by stackage.org.
   We need to put it in the *extra-deps* section of *stack.yaml*. Or you can run
   Stack solver to do that for you. Note that `stack solver` requires the `cabal`
   binary, which we need to install first:

   ```
   stack install cabal-install
   stack solver --update-config
   ```

   *Stack.yaml* is changed to:
   ```yaml
   flags: {}
   extra-package-dbs: []
   packages:
   - '.'
   extra-deps:
   - aeson-0.9.0.1
   - snaplet-sqlite-simple-0.4.8.3
   resolver: lts-6.7
   ```

   Make sure that the project builds.

2. Create the file *src/Memegen/Db.hs* which will hold the data model,
   and the data access layer.

3. Put the data model in it:

   ```haskell
   module Db where
   module Memegen.Db
       ( Meme(..)
       ) where

   import qualified Data.Text as T
   import qualified Snap.Snaplet.SqliteSimple as L

   data Meme = Meme
     {
       memeId :: Int
     , topText :: T.Text
     , bottomText :: T.Text
     , imageFilepath :: T.Text
     } deriving (Show)

   instance L.FromRow Meme where
     fromRow = Meme <$> L.field <*> L.field <*> L.field <*> L.field
   ```

4. Define the database schema, and create it if it doesn't exist:

   ```haskell
   {-# LANGUAGE OverloadedStrings #-}
   {-# LANGUAGE ScopedTypeVariables #-}

   module Memegen.Db
       ( Meme(..)
       , tableExists
       , createTables
       ) where

   import qualified Database.SQLite.Simple as D
   import           Control.Monad.State (unless)

   tableExists :: D.Connection -> String -> IO Bool
   tableExists conn tblName = do
     r <- D.query conn "SELECT name FROM sqlite_master \
                       \WHERE type='table' AND name=?" (L.Only tblName)
     case r of
       [L.Only (_ :: String)] -> return True
       _ -> return False

   -- | Create the necessary database tables, if not already initialized.
   createTables :: D.Connection -> IO ()
   createTables conn = do
     schemaCreated <- tableExists conn "memes"
     unless schemaCreated $
       D.execute_ conn
         (D.Query $ "CREATE TABLE memes (\
                    \id INTEGER PRIMARY KEY, \
                    \top_text TEXT, \
                    \bottom_text TEXT, \
                    \image_filepath TEXT)")
   ```

   Expose the `Memegen.Db` module and add *sqlite-simple* as a Cabal dependency:

   ```
   library
     hs-source-dirs:      src
     exposed-modules:     Memegen.Lib
                        , Memegen.App
                        , Memegen.Db
     build-depends:       base >= 4.7 && < 5
                        , sqlite-simple
                        ...
   ```

5. Write functions to read and write the database data:

   ```haskell
   module Memegen.Db
       ( Meme(..)
       , tableExists
       , createTables
       , listMemes
       , saveMeme
       ) where

   import Memegen.App (AppState(..))
   import Snap.Snaplet (Handler(..))

   -- | Retrieve all memes.
   listMemes :: Handler AppState L.Sqlite [Meme]
   listMemes = L.query "SELECT id, top_text, bottom_text, image_filepath \
                       \FROM memes ORDER BY id DESC" ()

   -- | Save a new meme
   saveMeme :: T.Text -> T.Text -> T.Text -> Handler AppState L.Sqlite ()
   saveMeme top bottom filepath =
     L.execute "INSERT INTO memes (top_text, bottom_text, image_filepath) \
               \VALUES (?, ?, ?)" (top, bottom, filepath)
   ```

   Notice that we now know type of our application in-memory state. It is
   `AppState` from *App.hs*. That is why we explicitly wrote it in
   `saveMeme` and `listMemes` type signatures.

6. Initialize the database in *Lib.hs*:

   ```haskell
   import qualified Memegen.Db as Db
   import qualified Snap.Snaplet.SqliteSimple as S
   import           Memegen.App (AppState(..), db)
   import           Control.Concurrent (withMVar)
   import           Control.Lens ((^#))

   appInit :: S.SnapletInit a ()
   appInit = S.makeSnaplet "memegen" "Meme generator." Nothing $ do
       S.addRoutes routes
       d <- S.nestSnaplet "db" db S.sqliteInit

       -- Grab the DB connection pool from the sqlite snaplet and call
       -- into the Model to create all the DB tables if necessary.
       let c = S.sqliteConn $ d ^# S.snapletValue
       liftIO $ withMVar c $ \conn -> Db.createTables conn

       -- Create upload directory
       liftIO $ createDirectoryIfMissing True "upload"

       return $ AppState d
   ```

   But this doesn't compile. The application now has a state. The type
   signature is wrong. Change it to:

   ```haskell
   appInit :: S.SnapletInit AppState AppState
   ```

   It works! Now that we know the application state type, let's update
   the remaining type signatures in *Lib.hs*:

   ```haskell
   -- Before
   routes :: [(B.ByteString, S.Handler a b ())]
   echoHandler :: S.Handler a b ()
   uploadHandler :: S.Handler a b ()

   -- After
   routes :: [(B.ByteString, S.Handler AppState AppState ())]
   echoHandler :: S.Handler AppState AppState ()
   uploadHandler :: S.Handler AppState AppState ()
   ```

7. Store the meme metadata at upload time. Hook the meme saving
   logic into `uploadHandler`:

   ```haskell
   import Data.Map.Lazy ((!))

   uploadHandler = do
     ...
     req <- S.getRequest
     let params = S.rqPostParams req
     let topText = decodeUtf8 $ head (params ! "top")
     let bottomText = decodeUtf8 $ head (params ! "bottom")
     S.withTop db $ Db.saveMeme topText bottomText (decodeUtf8 fileName)
     ...
     S.writeBS imgContent
   ```

   You need to add the *containers* library in the application's Cabal dependencies.


#### List of memes

We are able to store metadata in the database. But we still can't consume it.
Create a new handler which will list all stored memes:

1. We want to output the memes in JSON format. Enable JSON serialization for
   the `Meme` record in *Db.hs*:

   ```haskell
   {-# LANGUAGE TemplateHaskell #-}

   import Data.Aeson
   import Data.Aeson.TH

   $(deriveJSON defaultOptions ''Meme)
   ```

   This is TemplateHaskell-heavy code. It will generate the code required
   to serialize `Meme` record to JSON format.

   We are using Aeson library. Add *aeson* dependency in the Cabal config.

2. Add a new route in *Lib.hs*:

   ```haskell
   routes = [ ("/", S.ifTop $ S.writeBS "hello there")
            , ("hello/:echoparam", S.method S.GET echoHandler)
            , ("upload", S.method S.POST uploadHandler)
            , ("list", S.method S.GET listHandler)
            ]
   ```

2. Get the memes from the database and show them:

   ```haskell
   import qualified Data.ByteString.Lazy as BL
   import           Data.Aeson (encode)

   listHandler :: S.Handler AppState AppState ()
   listHandler = do
     memes <- S.withTop db $ Db.listMemes
     S.writeBS $ BL.toStrict $ encode memes
   ```

Upload a file using the existing HTML form. Then go to
[/list](http://localhost:8000/list) and you should see the stored memes.


#### Meme viewer

Listing memes gives you only metadata. We usually care about the picture.
Create a static content handler to enable image viewing.

Add a new route in *Lib.hs*:

```haskell
import qualified Snap.Util.FileServe as S

routes = [ ("/", S.ifTop $ S.writeBS "hello there")
         , ("hello/:echoparam", S.method S.GET echoHandler)
         , ("upload", S.method S.POST uploadHandler)
         , ("list", S.method S.GET listHandler)
         , ("image", S.serveDirectory "upload")
         ]
```

Done! Upload an image named *example.jpg*, then go to
[/image/example.jpg](http://localhost:8000/image/example.jpg) and it will
show you the uploaded image.


### Image processor

The next step in our Meme generator is to embed the top and bottom text into the
image. We will write a string onto the image using the well known
[GD library](https://libgd.github.io/pages/about.html).

1. Create the *src/Memegen/Img.hs* file with the following content:

   ```haskell
   module Memegen.Img
       ( createMeme
       ) where

   import qualified Graphics.GD as GD
   import qualified Data.ByteString as B

   textColor :: GD.Color
   textColor = GD.rgb 255 255 255

   textSize :: Double
   textSize = 32.0

   createMeme :: B.ByteString -> String -> String -> IO B.ByteString
   createMeme imgBs upperText lowerText = do
     img <- GD.loadJpegByteString imgBs
     (imgW, imgH) <- GD.imageSize img

     _ <- GD.useFontConfig True

     -- Draw upper text
     (_, (lrx, lry), _, (ulx, uly))
         <- GD.measureString "sans" textSize 0.0 (0, 0) upperText textColor
     let (textW, textH) = (1 + lrx - ulx, 1 + lry - uly)
     let upperPos = (imgW `div` 2 - textW `div` 2, textH + 10)
     _ <- GD.drawString "sans" textSize 0.0 upperPos upperText textColor img

     -- Draw lower text
     (_, (lrx, lry), _, (ulx, uly))
         <- GD.measureString "sans" textSize 0.0 (0, 0) lowerText textColor
     let (textW, textH) = (1 + lrx - ulx, 1 + lry - uly)
     let lowerPos = (imgW `div` 2 - textW `div` 2, imgH - 20)
     _ <- GD.drawString "sans" textSize 0.0 lowerPos lowerText textColor img

     GD.saveJpegByteString 100 img
   ```

   Note that this setup works only with *JPEG* images. The GD library supports
   more formats.

2. Expose the `Memegen.Img` module and add the *gd* library dependency:

   ```
   library
     hs-source-dirs:      src
     exposed-modules:     Memegen.Lib
                        , Memegen.App
                        , Memegen.Db
                        , Memegen.Img
     build-depends:       base >= 4.7 && < 5
                        , gd
                        ...
   ```

3. Hook up `createMeme` in the upload request handler:

   ```haskell
   import Memegen.Img (createMeme)

   uploadHandler = do
     ...
     memeContent <- liftIO $
       createMeme imgContent (T.unpack topText) (T.unpack bottomText)

     S.writeBS memeContent
   ```

   The final, complete, upload handler:

   ```haskell
   uploadHandler :: S.Handler AppState AppState ()
   uploadHandler = do
     files <- S.handleMultipart uploadPolicy $ \part -> do
       content <- liftM B.concat EL.consume
       return (part, content)
     let (imgPart, imgContent) = head files
     let fileName = fromJust (S.partFileName imgPart)

     req <- S.getRequest
     let params = S.rqPostParams req
     let topText = decodeUtf8 $ head (params ! "top")
     let bottomText = decodeUtf8 $ head (params ! "bottom")
     S.withTop db $ Db.saveMeme topText bottomText (decodeUtf8 fileName)

     memeContent <- liftIO $
       createMeme imgContent (T.unpack topText) (T.unpack bottomText)

     liftIO $ B.writeFile
       ("upload" </> (T.unpack $ decodeUtf8 fileName)) memeContent

     S.writeBS memeContent

     where
       uploadPolicy :: S.UploadPolicy
       uploadPolicy =
         S.setMaximumFormInputSize (2^(24::Int)) S.defaultUploadPolicy

   ```

We are done. Try it out!

*Note*: On Mac OS X, you might get an error message

```
Missing dependency on a foreign library:
    * Missing (or bad) header file: gd.h
```

when building with the gd dependency for the first time. You can solve this issue by building with the additional parameters ` --extra-lib-dirs=/usr/local/lib/ --extra-include-dirs=/usr/local/include/` (maybe you need to adapt this to your homebrew installation). These parameters only need to be added once, when the gd library is built.

## Disaster recovery

If you are stuck, consult this repository:
https://github.com/jaspervdj/haskell-beginners-projects/tree/master/memegen

If you missed a library dependency, here is the list of all of them:

```
aeson
bytestring
containers
directory
enumerator
filepath
gd
lens
mtl
snap
snap-core
snaplet-sqlite-simple
snap-server
sqlite-simple
text
```


## Followup ideas

* Write a test suite.
* Implement pagination for listing memes.
* Support more than one image format.
* Store image files under the hash of their content to properly deduplicate
  images and isolate sessions from each other.
* This code has a lot of shortcuts to ease understanding.
  Make the code production-ready!
* Make the code follow a good code style:
  https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
