module Interactions.Language where

import Control.Concurrent.STM
    ( TVar
    , modifyTVar
    , readTVarIO
    )
import Data.Map (insert, lookup)
import Data.Text (intercalate)
import Lucid
    ( Attribute
    , Html
    , Term (term)
    , ToHtml (toHtml)
    , body_
    , button_
    , charset_
    , class_
    , content_
    , crossorigin_
    , div_
    , h2_
    , head_
    , href_
    , html_
    , id_
    , integrity_
    , link_
    , meta_
    , name_
    , rel_
    , renderBS
    , src_
    , title_
    )
import Pages.Types (HTML, RawHtml (RawHtml))
import Protolude hiding (intercalate)
import Servant
    ( Get
    , Header
    , Headers
    , Post
    , QueryParam
    , Server
    , ToHttpApiData (toUrlPiece)
    , addHeader
    , type (:<|>) (..)
    , type (:>)
    )

-- | A newtype wrapper for a list of HX-Trigger values.
newtype Triggers = Triggers [Text]

instance ToHttpApiData Triggers where
    toUrlPiece (Triggers xs) = intercalate "," xs

-- | Type add an HX-Trigger header to a response.
type HxTriggerResponse a = Headers '[Header "HX-Trigger" Triggers] a

-- | Add an HX-Trigger header to a response.
addTriggerHeader :: [Text] -> a -> HxTriggerResponse a
addTriggerHeader = addHeader . Triggers

trigger :: Text -> Attribute
trigger = term "hx-trigger"

triggerFromBody :: Text -> Attribute
triggerFromBody x = term "hx-trigger" $ x <> " from:body"

getX :: Text -> Text -> Attribute
getX prefix path = term "hx-get" $ prefix <> "/" <> path

postX :: Text -> Text -> Attribute
postX prefix path = term "hx-post" $ prefix <> "/" <> path

target :: Text -> Attribute
target = term "hx-target"

click :: Attribute
click = trigger "click"

--------------------------------------------------------------------------------

-- * All3

--------------------------------------------------------------------------------

type All3API =
    "succ"
        :> QueryParam "id" Int
        :> Post '[HTML] (HxTriggerResponse RawHtml)
        :<|> "all3" :> Get '[HTML] RawHtml
        :<|> "count-all" :> Get '[HTML] RawHtml

all3H :: Text -> Map Int Int -> Html ()
all3H p s =
    forM_ [1 .. 3] $ \i -> oneB p s i

oneB :: Text -> Map Int Int -> Int -> Html ()
oneB p s = element
    do p
    do button_
    do \i -> "succ?id=" <> show i
    do toHtml @Text . show
    do \i -> toHtml @Text $ show $ fromMaybe 0 $ lookup i s

element
    :: Show a
    => Text
    -> ([Attribute] -> Html () -> Html ())
    -> (a -> Text)
    -> (a -> Html ())
    -> (a -> Html ())
    -> a
    -> Html ()
element p k u f q x = do
    k [target s, click, postX p $ u x] $ f x
    div_ [id_ r] $ q x
  where
    r = "s__" <> show x
    s = "#s__" <> show x

countAnyH :: Text -> Int -> Html ()
countAnyH p i = do
    div_ [triggerFromBody "count-any", getX p "count-all"] $ toHtml @Text $ show i

type All3S = TVar (Map Int Int)

all3 :: Text -> All3S -> Server All3API
all3 p s =
    do
        \case
            Just id -> do
                liftIO $ atomically $ modifyTVar s $ \m -> case lookup id m of
                    Nothing -> insert id 1 m
                    Just i -> insert id (i + 1) m
                m <- liftIO $ readTVarIO s
                pure
                    $ addTriggerHeader ["count-any"]
                    $ RawHtml
                    $ renderBS
                    $ toHtml @Text
                    $ show
                    $ fromMaybe 0
                    $ lookup id m
            Nothing ->
                pure
                    $ addTriggerHeader []
                    $ RawHtml
                    $ renderBS
                    $ toHtml @Text
                    $ "no id"
        :<|> do
            m <- liftIO $ readTVarIO s
            pure $ RawHtml $ renderBS $ pageH $ do
                div_ [class_ "row"] $ all3H p m
                h2_ [class_ "row"] $ countAnyH p 0
        :<|> do
            m <- liftIO $ readTVarIO s
            pure $ RawHtml $ renderBS $ toHtml @Text $ show $ sum m

pageH :: Html () -> Html ()
pageH body = html_ [term "data-bs-theme" "dark"]
    $ do
        head_ $ do
            title_ "htmx experiments"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_
                [ rel_ "stylesheet"
                , href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"
                , integrity_
                    "sha384-9ndCyUaIbzAi2FUVXJi0CjmCapSmO7SnpJef0486qhLnuZ2cdeRhO02iuK6FUUVM"
                , crossorigin_ "anonymous"
                ]
            term
                "script"
                [ src_
                    "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"
                , integrity_
                    "sha384-geWF76RCwLtnZ8qwWowPQNguL3RmwHVBC9FhGdlKrxdiJJigb/j/68SIy3Te4Bkz"
                , crossorigin_ "anonymous"
                ]
                $ pure ()
            term
                "script"
                [ src_ "https://unpkg.com/htmx.org@1.9.4"
                , integrity_ "sha384-zUfuhFKKZCbHTY6aRR46gxiqszMk5tcHjsVFxnUo8VMus4kHGVdIYVbOYYNlKmHV"
                , crossorigin_ "anonymous"
                ]
                $ pure ()
            -- <script src="https://unpkg.com/htmx.org/dist/ext/sse.js" integrity="sha384-9sPcBiA9fhU9fq7gfjFF29VlQp6vyoGP5skQ99zfpnpCEUZ2+9f+XmIk/DGE+ndH" crossorigin="anonymous"></script>
            term
                "script"
                [ src_ "https://unpkg.com/htmx.org/dist/ext/sse.js"
                , integrity_
                    "sha384-9sPcBiA9fhU9fq7gfjFF29VlQp6vyoGP5skQ99zfpnpCEUZ2+9f+XmIk/DGE+ndH"
                , crossorigin_ "anonymous"
                ]
                $ pure ()

        body_ $ do
            div_ [class_ "container-fluid"] body
