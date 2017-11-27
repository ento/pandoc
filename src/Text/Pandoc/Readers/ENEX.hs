module Text.Pandoc.Readers.ENEX (readEnex) where

import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Data.Default
import Data.Maybe (catMaybes, fromMaybe)
import Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Error
import Text.Pandoc.MediaBag (MediaBag)
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Shared (crFilter)
import Text.XML.Light

type ENEX m = StateT ENEXState m

data ENEXState = ENEXState { enexMediaBag :: MediaBag
                           } deriving Show

instance Default ENEXState where
  def = ENEXState { enexMediaBag = mempty
                  }

{-| Convert ENEX document to 'Pandoc' document.

Format of the input must conform to the DTD published by Evernote:
http://xml.evernote.com/pub/evernote-export3.dtd

= Mapping from ENEX elements to 'Pandoc' types

A `note` in ENEX becomes a 'Div' with class "note".

A single .enex file becomes a list of such 'Div's.

= List of supported and unsupported elements

en-export element

 - [x] element (note)+
 - [ ] attribute export-date
 - [ ] attribute application
 - [ ] attribute version

note element

 - [x] element title
 - [ ] element content
 - [ ] element created?
 - [ ] element updated?
 - [ ] element tag*
 - [ ] element note-attributes?
 - [ ] element resource*

note-attributes element

 - [ ] element subject-date?
 - [ ] element latitude?
 - [ ] element longitude?
 - [ ] element altitude?
 - [ ] element author?
 - [ ] element source?
 - [ ] element source-url?
 - [ ] element source-application?
 - [ ] element reminder-order?
 - [ ] element reminder-time?
 - [ ] element reminder-done-time?
 - [ ] element place-name?
 - [ ] element content-class?
 - [ ] element application-data*

resource element

 - [ ] element data
 - [ ] element mime
 - [ ] element width?
 - [ ] element height?
 - [ ] element duration?
 - [ ] element recognition?
 - [ ] element resource-attributes?
 - [ ] element alternate-data?

resource-attributes element

 - [ ] element source-url?
 - [ ] element timestamp?
 - [ ] element latitude?
 - [ ] element longitude?
 - [ ] element altitude?
 - [ ] element camera-make?
 - [ ] element camera-model?
 - [ ] element reco-type?
 - [ ] element file-name?
 - [ ] element attachment?
 - [ ] element application-data*

application-data element

 - [ ] attribute key
-}
readEnex :: PandocMonad m
         => ReaderOptions -- ^ Reader options
         -> Text        -- ^ String to parse (assumes @'\n'@ line endings)
         -> m Pandoc
readEnex opts inp = do
  let tree = parseXML $ T.unpack $ crFilter inp
  exports <- parseTopLevelElements $ onlyElems tree
  (bs, st') <- runStateT (mapM toPandoc exports) def
  return $ doc $ mconcat bs

class ToPandoc a where
  toPandoc :: PandocMonad m => a -> ENEX m Blocks

class ToPandocAttr a where
  toPandocAttr :: a -> B.Attr

data ENExport = ENExport
  { exportNotes :: [ENNote]
  , exportDate :: Maybe String
  , exportApplication :: Maybe String
  , exportVersion :: Maybe String
  } deriving (Show)

instance ToPandoc ENExport where
  toPandoc e = mconcat <$> mapM toPandoc (exportNotes e)

data ApplicationData = ApplicationData
  { applicationKey :: String
  , applicationData :: String
  } deriving (Show)

toPair :: ApplicationData -> (String, String)
toPair data' =
  (applicationKey data', applicationData data')

data ENNote = ENNote
  { noteTitle :: String
  , noteContent :: String
  , noteCreated :: Maybe String
  , noteUpdated :: Maybe String
  , noteTags :: [String]
  , noteAttributes :: NoteAttributes
  , noteResources :: [Resource]
  } deriving (Show)

instance ToPandoc ENNote where
  toPandoc e = return $ divWith noteAttr $ (header 1 $ (str . noteTitle) e)
    <> divWith nullAttr (para $ (str . noteContent) e)
    where noteAttr = ("", ["note"], [])

data NoteAttributes = NoteAttributes
  { noteSubjectDate :: Maybe String
  , noteLatitude :: Maybe Float
  , noteLongitude :: Maybe Float
  , noteAltitude :: Maybe Float
  , noteAuthor :: Maybe String
  , noteSource :: Maybe String
  , noteSourceUrl :: Maybe String
  , noteSourceApplication :: Maybe String
  , noteReminderOrder :: Maybe Int
  , noteReminderTime :: Maybe String
  , noteReminderDoneTime :: Maybe String
  , notePlaceName :: Maybe String
  , noteContentClass :: Maybe String
  , noteApplicationData :: [ApplicationData]
  } deriving (Show)

instance Default NoteAttributes where
  def = NoteAttributes
    { noteSubjectDate = Nothing
    , noteLatitude = Nothing
    , noteLongitude = Nothing
    , noteAltitude = Nothing
    , noteAuthor = Nothing
    , noteSource = Nothing
    , noteSourceUrl = Nothing
    , noteSourceApplication = Nothing
    , noteReminderOrder = Nothing
    , noteReminderTime = Nothing
    , noteReminderDoneTime = Nothing
    , notePlaceName = Nothing
    , noteContentClass = Nothing
    , noteApplicationData = []
    }

instance ToPandocAttr NoteAttributes where
  toPandocAttr attrs =
    let appDataPairs = toPair <$> noteApplicationData attrs
        pairs = catMaybes
          [ (,) "subject-date" <$> noteSubjectDate attrs
          , (,) "latitude" <$> show <$> noteLatitude attrs
          , (,) "longitude" <$> show <$> noteLongitude attrs
          , (,) "altitude" <$> show <$> noteAltitude attrs
          , (,) "author" <$> noteAuthor attrs
          , (,) "source" <$> noteSource attrs
          , (,) "source-url" <$> noteSourceUrl attrs
          , (,) "source-application" <$> noteSourceApplication attrs
          , (,) "reminder-order" <$> show <$> noteReminderOrder attrs
          , (,) "reminder-time" <$> noteReminderTime attrs
          , (,) "reminder-done-time" <$> noteReminderDoneTime attrs
          , (,) "place-name" <$> notePlaceName attrs
          , (,) "content-class" <$> noteContentClass attrs
          ]
    in ("", [], pairs ++ appDataPairs)

data Resource = Resource
  { resourceData :: String
  , resourceMime :: String
  , resourceWidth :: Int
  , resourceHeight :: Int
  , resourceDuration :: Int
  , resourceRecognition :: Maybe RecoIndex
  , resourceAttributes :: ResourceAttributes
  , resourceAlternateData :: Maybe String
  } deriving (Show)

data ResourceAttributes = ResourceAttributes
  { resourceSourceUrl :: Maybe String
  , resourceTimestamp :: Maybe String
  , resourceLatitude :: Maybe Float
  , resourceLongitude :: Maybe Float
  , resourceAltitude :: Maybe Float
  , resourceCameraMake :: Maybe String
  , resourceCameraModel :: Maybe String
  , resourceRecoType :: Maybe String
  , resourceFileName :: Maybe String
  , resourceAttachment :: Maybe Bool
  , resourceApplicationData :: [ApplicationData]
  } deriving (Show)

instance ToPandocAttr ResourceAttributes where
  toPandocAttr attrs =
    let appDataPairs = toPair <$> resourceApplicationData attrs
        pairs = catMaybes
          [ (,) "source-url" <$> resourceSourceUrl attrs
          , (,) "timestamp" <$> resourceTimestamp attrs
          , (,) "latitude" <$> show <$> resourceLatitude attrs
          , (,) "longitude" <$> show <$> resourceLongitude attrs
          , (,) "altitude" <$> show <$> resourceAltitude attrs
          , (,) "camera-make" <$> resourceCameraModel attrs
          , (,) "reco-type" <$> resourceRecoType attrs
          , (,) "file-name" <$> resourceFileName attrs
          , (,) "attachment" <$> show <$> resourceAttachment attrs
          ]
    in ("", [], pairs ++ appDataPairs)

data RecoIndex = RecoIndex
  {} deriving (Show)

instance ToPandoc RecoIndex where
  toPandoc e = return mempty

-- XML parsers

parseTopLevelElements :: PandocMonad m => [Element] -> m [ENExport]
parseTopLevelElements es = mapM parseExport $ filterExports es
  where filterExports = filter (\e -> "en-export" == qName (elName e))

parseExport :: PandocMonad m => Element -> m ENExport
parseExport e = do
  notes <- mapM parseNote $ findChildren (uqname "note") e
  return $ ENExport
    { exportNotes = notes
    , exportDate = Nothing
    , exportApplication = Nothing
    , exportVersion = Nothing
    }

parseNote :: PandocMonad m => Element -> m ENNote
parseNote e = do
  attrs <- fromMaybe (return def) $ parseNoteAttributes <$> findChild (uqname "note-attributes") e
  title <- strContent <$> findElementE (uqname "title") e
  content <- strContent <$> findElementE (uqname "content") e
  return $ ENNote
    { noteTitle = title
    , noteContent = content
    , noteCreated = Nothing
    , noteUpdated = Nothing
    , noteTags = []
    , noteAttributes = attrs
    , noteResources = []
    }

parseNoteAttributes :: PandocMonad m => Element -> m NoteAttributes
parseNoteAttributes e = return def

-- XML parsing helpers

-- UnQualified name
uqname :: String -> QName
uqname s = QName s Nothing Nothing

findElementE :: PandocMonad m => QName -> Element -> m Element
findElementE e x = mkE ("Unable to find element: " ++ show e) $ findElement e x

mkE :: PandocMonad m => String -> Maybe a -> m a
mkE s = maybe (throwError . PandocParseError $ s) return
