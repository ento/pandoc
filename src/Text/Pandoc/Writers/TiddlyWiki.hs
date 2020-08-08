{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.TiddlyWiki
   Copyright   : Copyright (C) 2020 github.com/ento
   License     : GNU GPL, version 2 or above

   Maintainer  : ento <ento+pandoc@i.pearlwaffles.xyz>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to TiddlyWiki markup.

TiddlyWiki:  <https://tiddlywiki.com/>
-}
module Text.Pandoc.Writers.TiddlyWiki ( writeTiddlyWiki ) where
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char (isAlphaNum, isSpace)
import Data.Default
import Data.List (find, intersperse, sortBy, transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP (urlEncode)
import Text.HTML.TagSoup (Tag (..), isTagText, parseTags)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, blanklines, char, space)
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.DocTemplates (Val(..), Context(..), FromContext(..))
import Text.Pandoc.Walk
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.Math (texMathToInlines)
import Data.Coerce (coerce)

type Notes = [[Block]]
type Ref   = (Text, Target, Attr)
type Refs  = [Ref]

type MD m = ReaderT WriterEnv (StateT WriterState m)

evalMD :: PandocMonad m => MD m a -> WriterEnv -> WriterState -> m a
evalMD md env st = evalStateT (runReaderT md env) st

data WriterEnv = WriterEnv { envInList          :: Bool
                           , envBlockLevel      :: Int
                           , envEscapeSpaces    :: Bool
                           }

instance Default WriterEnv
  where def = WriterEnv { envInList          = False
                        , envBlockLevel      = 0
                        , envEscapeSpaces    = False
                        }

data WriterState = WriterState { stNotes   :: Notes
                               , stPrevRefs :: Refs
                               , stRefs    :: Refs
                               , stKeys    :: M.Map Key
                                                (M.Map (Target, Attr) Int)
                               , stLastIdx  :: Int
                               , stIds     :: Set.Set Text
                               , stNoteNum :: Int
                               }

instance Default WriterState
  where def = WriterState{ stNotes = []
                         , stPrevRefs = []
                         , stRefs = []
                         , stKeys = M.empty
                         , stLastIdx = 0
                         , stIds = Set.empty
                         , stNoteNum = 1
                         }

-- | Convert Pandoc to TiddlyWiki.
writeTiddlyWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTiddlyWiki opts document =
  evalMD (pandocToTiddlyWiki opts document) def def

tiddlyWikiFieldsBlock :: Context Text -> Doc Text
tiddlyWikiFieldsBlock (Context hashmap) =
  vcat $ map go $ sortBy (comparing fst) $ M.toList hashmap
  where go (k,v) =
          case (text (T.unpack k), v) of
               (k', ListVal xs)
                 | null xs        -> empty
                 | otherwise      -> k' <> ":" <> space <>
                                      hcat (intersperse "; " $
                                            mapMaybe fromVal xs)
               (k', SimpleVal x)
                      | isEmpty x -> empty
                      | otherwise -> k' <> ":" <> space <>
                                     nest 2 (removeBlankLines (chomp x))
               _                  -> empty
        removeBlankLines BlankLines{} = cr <> text "." <> cr
        removeBlankLines (Concat x y) = removeBlankLines x <>
                                        removeBlankLines y
        removeBlankLines x            = x

-- | Return tiddlywiki representation of document.
pandocToTiddlyWiki :: PandocMonad m => WriterOptions -> Pandoc -> MD m Text
pandocToTiddlyWiki opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  metadata <- metaToContext'
               (blockListToTiddlyWiki opts)
               (inlineListToTiddlyWiki opts)
               meta
  let titleblock = case writerTemplate opts of
                        Just _ -> tiddlyWikiFieldsBlock metadata
                        Nothing -> empty
  toc <- if writerTableOfContents opts
         then blockToTiddlyWiki opts ( toTableOfContents opts blocks )
         else return mempty
  body <- blockListToTiddlyWiki opts blocks
  notesAndRefs' <- notesAndRefs opts
  let main = body <> notesAndRefs'
  let context  = -- for backwards compatibility we populate toc
                 -- with the contents of the toc, rather than a
                 -- boolean:
                 defField "toc" toc
               $ defField "table-of-contents" toc
               $ defField "body" main
               $ (if isNullMeta meta
                     then id
                     else defField "titleblock" titleblock)
               $ addVariablesToContext opts metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

-- | Return tiddlywiki representation of reference key table.
refsToTiddlyWiki :: PandocMonad m => WriterOptions -> Refs -> MD m (Doc Text)
refsToTiddlyWiki opts refs = mapM (keyToTiddlyWiki opts) refs >>= return . vcat

-- | Return tiddlywiki representation of a reference key.
keyToTiddlyWiki :: PandocMonad m
              => WriterOptions
              -> Ref
              -> MD m (Doc Text)
keyToTiddlyWiki opts (label', (src, tit), attr) = do
  let tit' = if T.null tit
                then empty
                else space <> "\"" <> literal tit <> "\""
  return $ nest 2 $ hang 2
            ("[" <> literal label' <> "]:" <> space) (literal src <> tit')
            <+> linkAttributes opts attr

-- | Return tiddlywiki representation of notes.
notesToTiddlyWiki :: PandocMonad m => WriterOptions -> [[Block]] -> MD m (Doc Text)
notesToTiddlyWiki opts notes = do
  n <- gets stNoteNum
  notes' <- zipWithM (noteToTiddlyWiki opts) [n..] notes
  modify $ \st -> st { stNoteNum = stNoteNum st + length notes }
  return $ vsep notes'

-- | Return tiddlywiki representation of a note.
noteToTiddlyWiki :: PandocMonad m => WriterOptions -> Int -> [Block] -> MD m (Doc Text)
noteToTiddlyWiki opts num blocks = do
  contents  <- blockListToTiddlyWiki opts blocks
  let num' = literal $ writerIdentifierPrefix opts <> tshow num
  let marker = literal "[" <> num' <> literal "]"
  let markerSize = 4 + offset num'
  let spacer = case writerTabStop opts - markerSize of
                     n | n > 0  -> literal $ T.replicate n " "
                     _ -> literal " "
  return $ marker <> spacer <> contents

-- | Escape special characters for TiddlyWiki.
escapeText :: WriterOptions -> Text -> Text
escapeText opts = T.pack . go . T.unpack
  where
  go [] = []
  go (c:cs) =
    case c of
       '<' | isEnabled Ext_all_symbols_escapable opts ->
              '\\' : '<' : go cs
           | otherwise -> "&lt;" ++ go cs
       '>' | isEnabled Ext_all_symbols_escapable opts ->
              '\\' : '>' : go cs
           | otherwise -> "&gt;" ++ go cs
       _ | c `elem` ['\\','`','*','_','[',']','#'] ->
              "<$text text='" ++ c:"'/>" ++ go cs
       '|' | isEnabled Ext_pipe_tables opts -> '\\':'|':go cs
       '^' -> "<$text text='^'/>" ++ go cs
       '~' -> "<$text text='~'/>" ++ go cs
       '$' | isEnabled Ext_tex_math_dollars opts -> "<$text text='$'/>" ++ go cs
       _   -> case cs of
                '_':x:xs
                  | isEnabled Ext_intraword_underscores opts
                  , isAlphaNum c
                  , isAlphaNum x -> c : '_' : x : go xs
                _                -> c : go cs

attrsToTiddlyWiki :: Attr -> Doc Text
attrsToTiddlyWiki attribs = braces $ hsep [attribId, attribClasses, attribKeys]
        where attribId = case attribs of
                                ("",_,_) -> empty
                                (i,_,_)  -> "#" <> escAttr i
              attribClasses = case attribs of
                                (_,[],_) -> empty
                                (_,cs,_) -> hsep $
                                            map (escAttr . ("."<>))
                                            cs
              attribKeys = case attribs of
                                (_,_,[]) -> empty
                                (_,_,ks) -> hsep $
                                            map (\(k,v) -> escAttr k
                                              <> "=\"" <>
                                              escAttr v <> "\"") ks
              escAttr          = mconcat . map escAttrChar . T.unpack
              escAttrChar '"'  = literal "\\\""
              escAttrChar '\\' = literal "\\\\"
              escAttrChar c    = literal $ T.singleton c

styleAttrsToTiddlyWiki :: Attr -> Doc Text
styleAttrsToTiddlyWiki attribs =
  hcat $
  intersperse ";" $
  filter (not . isEmpty) [attribStyle, attribClasses]
  where attribClasses = case attribs of
                          (_,[],_) -> empty
                          (_,cs,_) -> hsep $ map (literal . ("."<>)) cs
        attribStyle = case attribs of
                       (_,_,[]) -> empty
                       (_,_,ks) ->
                         case find (\(k,_) -> k == "style") ks of
                           Just (_,style) -> literal $
                             T.pack . filter (not . isSpace) . T.unpack $ style
                           Nothing -> empty

linkAttributes :: WriterOptions -> Attr -> Doc Text
linkAttributes opts attr =
  if isEnabled Ext_link_attributes opts && attr /= nullAttr
     then attrsToTiddlyWiki attr
     else empty

-- | Ordered list start parser for use in Para below.
olMarker :: Parser Text ParserState ()
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period &&
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then mzero -- it needs 2 spaces anyway
                          else eof

-- | True if string begins with an ordered list marker
beginsWithOrderedListMarker :: Text -> Bool
beginsWithOrderedListMarker str =
  case runParser olMarker defaultParserState "para start" (T.take 10 str) of
         Left  _ -> False
         Right _ -> True

notesAndRefs :: PandocMonad m => WriterOptions -> MD m (Doc Text)
notesAndRefs opts = do
  notes' <- gets stNotes >>= notesToTiddlyWiki opts . reverse
  modify $ \s -> s { stNotes = [] }
  refs' <- gets stRefs >>= refsToTiddlyWiki opts . reverse
  modify $ \s -> s { stPrevRefs = stPrevRefs s ++ stRefs s
                   , stRefs = []}

  let endSpacing =
        if | writerReferenceLocation opts == EndOfDocument -> empty
           | isEmpty notes' && isEmpty refs' -> empty
           | otherwise -> blankline

  return $
    (if isEmpty notes' then empty else blankline <> notes') <>
    (if isEmpty refs' then empty else blankline <> refs') <>
    endSpacing

-- | Convert Pandoc block element to tiddlywiki.
blockToTiddlyWiki :: PandocMonad m
                => WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> MD m (Doc Text)
blockToTiddlyWiki opts blk =
  local (\env -> env {envBlockLevel = envBlockLevel env + 1}) $
  do doc <- blockToTiddlyWiki' opts blk
     blkLevel <- asks envBlockLevel
     if writerReferenceLocation opts == EndOfBlock && blkLevel == 1
       then notesAndRefs opts >>= (\d -> return $ doc <> d)
       else return doc

blockToTiddlyWiki' :: PandocMonad m
                 => WriterOptions -- ^ Options
                 -> Block         -- ^ Block element
                 -> MD m (Doc Text)
blockToTiddlyWiki' _ Null = return empty
blockToTiddlyWiki' opts (Div attrs ils) = do
  contents <- blockListToTiddlyWiki opts ils
  return $
    case () of
         _ | isEnabled Ext_fenced_divs opts &&
             attrs /= nullAttr ->
                nowrap (literal ":::" <+> attrsToTiddlyWiki attrs) $$
                chomp contents $$
                literal ":::" <> blankline
           | isEnabled Ext_native_divs opts ||
             (isEnabled Ext_raw_html opts &&
              isEnabled Ext_markdown_in_html_blocks opts) ->
                tagWithAttrs "div" attrs <> blankline <>
                contents <> blankline <> "</div>" <> blankline
           | isEnabled Ext_raw_html opts &&
             isEnabled Ext_markdown_attribute opts ->
                tagWithAttrs "div" attrs' <> blankline <>
                contents <> blankline <> "</div>" <> blankline
           | otherwise -> contents <> blankline
       where (id',classes',kvs') = attrs
             attrs' = (id',classes',("tiddlywiki","1"):kvs')
blockToTiddlyWiki' opts (Plain inlines) = do
  -- escape if para starts with ordered list marker
  let escapeMarker = T.concatMap $ \x -> if x `elemText` ".()"
                                         then T.pack ['\\', x]
                                         else T.singleton x
  let startsWithSpace (Space:_)     = True
      startsWithSpace (SoftBreak:_) = True
      startsWithSpace _             = False
  let inlines' =
        case inlines of
          (Str t:ys)
            | (null ys || startsWithSpace ys)
            , beginsWithOrderedListMarker t
              -> RawInline (Format "tiddlywiki") (escapeMarker t):ys
          (Str t:_)
            | t == "+" || t == "-"
              -> RawInline (Format "tiddlywiki") "\\" : inlines
          _ -> inlines
  contents <- inlineListToTiddlyWiki opts inlines'
  return $ contents <> cr
-- title beginning with fig: indicates figure
blockToTiddlyWiki' opts (Para [Image attr alt (src,tgt@(T.stripPrefix "fig:" -> Just tit))])
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    ((<> blankline) . literal . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing }
        (Pandoc nullMeta [Para [Image attr alt (src,tgt)]])
  | otherwise = blockToTiddlyWiki opts (Para [Image attr alt (src,tit)])
blockToTiddlyWiki' opts (Para inlines) =
  (<> blankline) `fmap` blockToTiddlyWiki opts (Plain inlines)
blockToTiddlyWiki' opts (LineBlock lns) = do
  let docify line = if null line
                    then return blankline
                    else inlineListToTiddlyWiki opts line
  let joinWithLinefeeds = nowrap . mconcat . intersperse cr
  contents <- joinWithLinefeeds <$> mapM docify lns
  return $ literal "\"\"\"" $$ contents $$ literal "\"\"\"" $$ blankline
blockToTiddlyWiki' opts b@(RawBlock f str) = do
  let Format fmt = f
  render' fmt
  where
    rawAttribBlock fmt =
      return $ (literal "```" <> literal fmt) $$
      literal str $$
      (literal "```" <> literal "\n")
    renderEmpty = mempty <$ report (BlockNotRendered b)
    render' fmt
      | f `elem` ["tiddlywiki"] =
          return $ literal str <> literal "\n"
      | isEnabled Ext_raw_attribute opts = rawAttribBlock fmt
      | f `elem` ["html", "html5", "html4"] =
          case () of
            _ | isEnabled Ext_markdown_attribute opts ->
                return $ literal (addTiddlyWikiAttribute str) <> literal "\n"
              | isEnabled Ext_raw_html opts ->
                return $ literal str <> literal "\n"
              | isEnabled Ext_raw_attribute opts -> rawAttribBlock fmt
              | otherwise -> renderEmpty
      | f `elem` ["latex", "tex"] =
          case () of
            _ | isEnabled Ext_raw_attribute opts -> rawAttribBlock fmt
              | otherwise -> renderEmpty
      | otherwise = renderEmpty
blockToTiddlyWiki' _opts HorizontalRule =
  return $ blankline <> literal (T.replicate 3 "-") <> blankline
blockToTiddlyWiki' opts (Header level attr inlines) = do
  -- first, if we're putting references at the end of a section, we
  -- put them here.
  blkLevel <- asks envBlockLevel
  refs <- if writerReferenceLocation opts == EndOfSection && blkLevel == 1
          then notesAndRefs opts
          else return empty

  -- we calculate the id that would be used by auto_identifiers
  -- so we know whether to print an explicit identifier
  ids <- gets stIds
  let autoId = uniqueIdent (writerExtensions opts) inlines ids
  modify $ \st -> st{ stIds = Set.insert autoId ids }
  let attr' = case attr of
                   ("",[],[]) -> empty
                   (id',[],[]) | isEnabled Ext_auto_identifiers opts
                                 && id' == autoId -> empty
                   (id',_,_)   | isEnabled Ext_mmd_header_identifiers opts ->
                                    space <> brackets (literal id')
                   _ | isEnabled Ext_header_attributes opts ->
                                    space <> attrsToTiddlyWiki attr
                     | otherwise -> empty
  contents <- inlineListToTiddlyWiki opts $
                 -- ensure no newlines; see #3736
                 walk lineBreakToSpace $ inlines
  let hdr = nowrap $ literal (T.replicate level "!") <> space <> contents <> attr' <> blankline

  return $ refs <> hdr
blockToTiddlyWiki' _ (CodeBlock attribs str) = return $
  backticks <> attrs <> cr <> literal str <> cr <> backticks <> blankline
  where endline c = literal $ case [T.length ln
                                   | ln <- map trim (T.lines str)
                                   , T.pack [c,c,c] `T.isPrefixOf` ln
                                   , T.all (== c) ln] of
                               [] -> T.replicate 3 $ T.singleton c
                               xs -> T.replicate (maximum xs + 1) $ T.singleton c
        backticks = endline '`'
        attrs  = case attribs of
                   (_,(cls:_),_) -> " " <> literal cls
                   _             -> empty
blockToTiddlyWiki' opts (BlockQuote blocks) = do
  let leader = "> "
  contents <- blockListToTiddlyWiki opts blocks
  return $ (prefixed leader contents) <> blankline
blockToTiddlyWiki' opts t@(Table _ blkCapt specs thead tbody tfoot) = do
  let (caption, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  let numcols = maximum (length aligns : length widths :
                           map length (headers:rows))
  caption' <- inlineListToTiddlyWiki opts caption
  let caption'' = if null caption
                     then blankline
                     else
                       if isEnabled Ext_table_captions opts
                          then blankline $$ (": " <> caption') $$ blankline
                          else blankline $$ caption' $$ blankline
  let hasSimpleCells = onlySimpleTableCells $ headers : rows
  let isSimple = hasSimpleCells && all (==0) widths
  let isPlainBlock (Plain _) = True
      isPlainBlock _         = False
  let hasBlocks = not (all isPlainBlock $ concat . concat $ headers:rows)
  let padRow r = case numcols - length r of
                       x | x > 0 -> r ++ replicate x empty
                         | otherwise -> r
  let aligns' = case numcols - length aligns of
                     x | x > 0 -> aligns ++ replicate x AlignDefault
                       | otherwise -> aligns
  let widths' = case numcols - length widths of
                     x | x > 0 -> widths ++ replicate x 0.0
                       | otherwise -> widths
  (nst,tbl) <-
     case True of
          _ | isSimple &&
              isEnabled Ext_simple_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToTiddlyWiki opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToTiddlyWiki opts))
                           rows
                (nest 2,) <$> pandocTable opts False (all null headers)
                                aligns' widths' rawHeaders rawRows
            | isSimple &&
              isEnabled Ext_pipe_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToTiddlyWiki opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToTiddlyWiki opts))
                           rows
                (id,) <$> pipeTable (all null headers) aligns' rawHeaders rawRows
            | not hasBlocks &&
              isEnabled Ext_multiline_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToTiddlyWiki opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToTiddlyWiki opts))
                           rows
                (nest 2,) <$> pandocTable opts True (all null headers)
                                aligns' widths' rawHeaders rawRows
            | isEnabled Ext_grid_tables opts &&
               writerColumns opts >= 8 * numcols -> (id,) <$>
                gridTable opts blockListToTiddlyWiki
                  (all null headers) aligns' widths' headers rows
            | hasSimpleCells &&
              isEnabled Ext_pipe_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToTiddlyWiki opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToTiddlyWiki opts))
                           rows
                (id,) <$> pipeTable (all null headers) aligns' rawHeaders rawRows
            | isEnabled Ext_raw_html opts -> fmap (id,) $
                   literal <$>
                   (writeHtml5String opts{ writerTemplate = Nothing } $ Pandoc nullMeta [t])
            | otherwise -> return (id, literal "[TABLE]")
  return $ nst (tbl $$ caption'') $$ blankline
blockToTiddlyWiki' opts (BulletList items) = do
  contents <- inList $ mapM (bulletListItemToTiddlyWiki opts) items
  return $ (if isTightList items then vcat else vsep) contents <> blankline
blockToTiddlyWiki' opts (OrderedList (start,_,_) items) = do
  let start' = if isEnabled Ext_startnum opts then start else 1
  let sty    = DefaultStyle
  let delim  = DefaultDelim
  let attribs = (start', sty, delim)
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if T.length m < 3
                               then m <> T.replicate (3 - T.length m) " "
                               else m) markers
  contents <- inList $
              zipWithM (orderedListItemToTiddlyWiki opts) markers' items
  return $ (if isTightList items then vcat else vsep) contents <> blankline
blockToTiddlyWiki' opts (DefinitionList items) = do
  contents <- inList $ mapM (definitionListItemToTiddlyWiki opts) items
  return $ mconcat contents

inList :: Monad m => MD m a -> MD m a
inList p = local (\env -> env {envInList = True}) p

addTiddlyWikiAttribute :: Text -> Text
addTiddlyWikiAttribute s =
  case span isTagText $ reverse $ parseTags s of
       (xs,(TagOpen t attrs:rest)) ->
            renderTags' $ reverse rest ++ (TagOpen t attrs' : reverse xs)
              where attrs' = ("tiddlywiki","1"):[(x,y) | (x,y) <- attrs,
                                 x /= "tiddlywiki"]
       _ -> s

pipeTable :: PandocMonad m
          => Bool -> [Alignment] -> [Doc Text] -> [[Doc Text]]
          -> MD m (Doc Text)
pipeTable headless aligns rawHeaders rawRows = do
  let sp = literal " "
  let blockFor AlignLeft   x y = lblock (x + 2) (sp <> y) <> lblock 0 empty
      blockFor AlignCenter x y = cblock (x + 2) (sp <> y <> sp) <> lblock 0 empty
      blockFor AlignRight  x y = rblock (x + 2) (y <> sp) <> lblock 0 empty
      blockFor _           x y = lblock (x + 2) (sp <> y) <> lblock 0 empty
  let widths = map (max 3 . maximum . map offset) $ transpose (rawHeaders : rawRows)
  let torow cs = nowrap $ literal "|" <>
                    hcat (intersperse (literal "|") $
                          zipWith3 blockFor aligns widths (map chomp cs))
                    <> literal "|"
  let toborder a w = literal $ case a of
                          AlignLeft    -> ":" <> T.replicate (w + 1) "-"
                          AlignCenter  -> ":" <> T.replicate w "-" <> ":"
                          AlignRight   -> T.replicate (w + 1) "-" <> ":"
                          AlignDefault -> T.replicate (w + 2) "-"
  -- note:  pipe tables can't completely lack a
  -- header; for a headerless table, we need a header of empty cells.
  -- see jgm/pandoc#1996.
  let header = if headless
                  then torow (replicate (length aligns) empty)
                  else torow rawHeaders
  let border = nowrap $ literal "|" <> hcat (intersperse (literal "|") $
                        zipWith toborder aligns widths) <> literal "|"
  let body   = vcat $ map torow rawRows
  return $ header $$ border $$ body

pandocTable :: PandocMonad m
            => WriterOptions -> Bool -> Bool -> [Alignment] -> [Double]
            -> [Doc Text] -> [[Doc Text]] -> MD m (Doc Text)
pandocTable opts multiline headless aligns widths rawHeaders rawRows = do
  let isSimple = all (==0) widths
  let alignHeader alignment = case alignment of
                                AlignLeft    -> lblock
                                AlignCenter  -> cblock
                                AlignRight   -> rblock
                                AlignDefault -> lblock
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break.
  -- The @+2@ is needed for specifying the alignment.
  let numChars    = (+ 2) . maximum . map offset
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break *inside a word*.
  -- The @+2@ is needed for specifying the alignment.
  let minNumChars = (+ 2) . maximum . map minOffset
  let columns = transpose (rawHeaders : rawRows)
  -- minimal column width without wrapping a single word
  let relWidth w col =
         max (floor $ fromIntegral (writerColumns opts - 1) * w)
             (if writerWrapText opts == WrapAuto
                 then minNumChars col
                 else numChars col)
  let widthsInChars
        | isSimple  = map numChars columns
        | otherwise = zipWith relWidth widths columns
  let makeRow = hcat . intersperse (lblock 1 (literal " ")) .
                   (zipWith3 alignHeader aligns widthsInChars)
  let rows' = map makeRow rawRows
  let head' = makeRow rawHeaders
  let underline = mconcat $ intersperse (literal " ") $
                  map (\width -> literal (T.replicate width "-")) widthsInChars
  let border = if multiline
                  then literal (T.replicate (sum widthsInChars +
                          length widthsInChars - 1) "-")
                  else if headless
                          then underline
                          else empty
  let head'' = if headless
                  then empty
                  else border <> cr <> head'
  let body = if multiline
                then vsep rows' $$
                     if length rows' < 2
                        then blankline -- #4578
                        else empty
                else vcat rows'
  let bottom = if headless
                  then underline
                  else border
  return $ head'' $$ underline $$ body $$ bottom

itemEndsWithTightList :: [Block] -> Bool
itemEndsWithTightList bs =
  case bs of
        [Plain _, BulletList xs]    -> isTightList xs
        [Plain _, OrderedList _ xs] -> isTightList xs
        _                           -> False

-- | Convert bullet list item (list of blocks) to tiddlywiki.
bulletListItemToTiddlyWiki :: PandocMonad m => WriterOptions -> [Block] -> MD m (Doc Text)
bulletListItemToTiddlyWiki opts bs = do
  let exts = writerExtensions opts
  contents <- blockListToTiddlyWiki opts $ taskListItemToAscii exts bs
  let sps = T.replicate (writerTabStop opts - 2) " "
  let start = literal $ "- " <> sps
  -- remove trailing blank line if item ends with a tight list
  let contents' = if itemEndsWithTightList bs
                     then chomp contents <> cr
                     else contents
  return $ hang (writerTabStop opts) start contents'

-- | Convert ordered list item (a list of blocks) to tiddlywiki.
orderedListItemToTiddlyWiki :: PandocMonad m
                          => WriterOptions -- ^ options
                          -> Text        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> MD m (Doc Text)
orderedListItemToTiddlyWiki opts marker bs = do
  let exts = writerExtensions opts
  contents <- blockListToTiddlyWiki opts $ taskListItemToAscii exts bs
  let sps = case writerTabStop opts - T.length marker of
                   n | n > 0 -> literal $ T.replicate n " "
                   _ -> literal " "
  let ind = if isEnabled Ext_four_space_rule opts
               then writerTabStop opts
               else max (writerTabStop opts) (T.length marker + 1)
  let start = literal marker <> sps
  -- remove trailing blank line if item ends with a tight list
  let contents' = if itemEndsWithTightList bs
                     then chomp contents <> cr
                     else contents
  return $ hang ind start contents'

-- | Convert definition list item (label, list of blocks) to tiddlywiki.
definitionListItemToTiddlyWiki :: PandocMonad m
                             => WriterOptions
                             -> ([Inline],[[Block]])
                             -> MD m (Doc Text)
definitionListItemToTiddlyWiki opts (label, defs) = do
  labelText <- blockToTiddlyWiki opts (Plain label)
  defs' <- mapM (mapM (blockToTiddlyWiki opts)) defs
  return $ ": " <> nowrap labelText <> cr <> vcat (map renderDef defs') <> cr
  where renderDef blocks =
          let
            contents = vcat blocks
          in
            "; " <> if height contents < 2
                    then contents
                    else literal "<div>" <> blankline <> contents <> literal "</div>"

-- | Convert list of Pandoc block elements to tiddlywiki.
blockListToTiddlyWiki :: PandocMonad m
                    => WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> MD m (Doc Text)
blockListToTiddlyWiki opts blocks = do
  inlist <- asks envInList
  -- a) insert comment between list and indented code block, or the
  -- code block will be treated as a list continuation paragraph
  -- b) change Plain to Para unless it's followed by a RawBlock
  -- or has a list as its parent (#3487)
  let fixBlocks (b : CodeBlock attr x : rest)
         | (not (isEnabled Ext_fenced_code_blocks opts) || attr == nullAttr)
             && isListBlock b = b : commentSep : CodeBlock attr x :
                                fixBlocks rest
      fixBlocks (b1@(BulletList _) : b2@(BulletList _) : bs) =
           b1 : commentSep : fixBlocks (b2:bs)
      fixBlocks (b1@(OrderedList _ _) : b2@(OrderedList _ _) : bs) =
           b1 : commentSep : fixBlocks (b2:bs)
      fixBlocks (b1@(DefinitionList _) : b2@(DefinitionList _) : bs) =
           b1 : commentSep : fixBlocks (b2:bs)
      fixBlocks (Plain ils : bs@(RawBlock{}:_)) =
           Plain ils : fixBlocks bs
      fixBlocks (Plain ils : bs) | inlist =
           Plain ils : fixBlocks bs
      fixBlocks (Plain ils : bs) =
           Para ils : fixBlocks bs
      fixBlocks (r@(RawBlock f raw) : b : bs)
        | not (T.null raw)
        , T.last raw /= '\n' =
        case b of
             Plain{}    -> r : fixBlocks (b:bs)
             RawBlock{} -> r : fixBlocks (b:bs)
             _          -> RawBlock f (raw <> "\n") : fixBlocks (b:bs) -- #4629
      fixBlocks (x : xs)             = x : fixBlocks xs
      fixBlocks []                   = []
      isListBlock (BulletList _)     = True
      isListBlock (OrderedList _ _)  = True
      isListBlock (DefinitionList _) = True
      isListBlock _                  = False
      commentSep  = if isEnabled Ext_raw_html opts
                    then RawBlock "html" "<!-- -->\n"
                    else RawBlock "tiddlywiki" "&nbsp;\n"
  mapM (blockToTiddlyWiki opts) (fixBlocks blocks) >>= return . mconcat

getKey :: Doc Text -> Key
getKey = toKey . render Nothing

findUsableIndex :: [Text] -> Int -> Int
findUsableIndex lbls i = if (tshow i) `elem` lbls
                         then findUsableIndex lbls (i + 1)
                         else i

getNextIndex :: PandocMonad m => MD m Int
getNextIndex = do
  prevRefs <- gets stPrevRefs
  refs <- gets stRefs
  i <- (+ 1) <$> gets stLastIdx
  modify $ \s -> s{ stLastIdx = i }
  let refLbls = map (\(r,_,_) -> r) $ prevRefs ++ refs
  return $ findUsableIndex refLbls i

-- | Get reference for target; if none exists, create unique one and return.
--   Prefer label if possible; otherwise, generate a unique key.
getReference :: PandocMonad m => Attr -> Doc Text -> Target -> MD m Text
getReference attr label target = do
  refs <- gets stRefs
  case find (\(_,t,a) -> t == target && a == attr) refs of
    Just (ref, _, _) -> return ref
    Nothing       -> do
      keys <- gets stKeys
      let key = getKey label
      let rawkey = coerce key
      case M.lookup key keys of
           Nothing -> do -- no other refs with this label
             (lab', idx) <- if T.null rawkey ||
                                 T.length rawkey > 999 ||
                                 T.any (\c -> c == '[' || c == ']') rawkey
                               then do
                                 i <- getNextIndex
                                 return (tshow i, i)
                               else
                                 return (render Nothing label, 0)
             modify (\s -> s{
               stRefs = (lab', target, attr) : refs,
               stKeys = M.insert (getKey label)
                           (M.insert (target, attr) idx mempty)
                                 (stKeys s) })
             return lab'

           Just km ->    -- we have refs with this label
             case M.lookup (target, attr) km of
                  Just i -> do
                    let lab' = render Nothing $
                               label <> if i == 0
                                           then mempty
                                           else literal (tshow i)
                    -- make sure it's in stRefs; it may be
                    -- a duplicate that was printed in a previous
                    -- block:
                    when ((lab', target, attr) `notElem` refs) $
                       modify (\s -> s{
                         stRefs = (lab', target, attr) : refs })
                    return lab'
                  Nothing -> do -- but this one is to a new target
                    i <- getNextIndex
                    let lab' = tshow i
                    modify (\s -> s{
                       stRefs = (lab', target, attr) : refs,
                       stKeys = M.insert key
                                   (M.insert (target, attr) i km)
                                         (stKeys s) })
                    return lab'

-- | Convert list of Pandoc inline elements to tiddlywiki.
inlineListToTiddlyWiki :: PandocMonad m => WriterOptions -> [Inline] -> MD m (Doc Text)
inlineListToTiddlyWiki opts lst = do
  inlist <- asks envInList
  go (if inlist then avoidBadWrapsInList lst else lst)
  where go [] = return empty
        go (i:is) = case i of
            (Link _ _ _) -> case is of
                -- If a link is followed by another link, or '[', '(' or ':'
                -- then we don't shortcut
                (Link _ _ _):_                                  -> unshortcutable
                Space:(Link _ _ _):_                            -> unshortcutable
                Space:(Str(thead -> Just '[')):_                -> unshortcutable
                Space:(RawInline _ (thead -> Just '[')):_       -> unshortcutable
                Space:(Cite _ _):_                              -> unshortcutable
                SoftBreak:(Link _ _ _):_                        -> unshortcutable
                SoftBreak:(Str(thead -> Just '[')):_            -> unshortcutable
                SoftBreak:(RawInline _ (thead -> Just '[')):_   -> unshortcutable
                SoftBreak:(Cite _ _):_                          -> unshortcutable
                LineBreak:(Link _ _ _):_                        -> unshortcutable
                LineBreak:(Str(thead -> Just '[')):_            -> unshortcutable
                LineBreak:(RawInline _ (thead -> Just '[')):_   -> unshortcutable
                LineBreak:(Cite _ _):_                          -> unshortcutable
                (Cite _ _):_                                    -> unshortcutable
                Str (thead -> Just '['):_                       -> unshortcutable
                Str (thead -> Just '('):_                       -> unshortcutable
                Str (thead -> Just ':'):_                       -> unshortcutable
                (RawInline _ (thead -> Just '[')):_             -> unshortcutable
                (RawInline _ (thead -> Just '(')):_             -> unshortcutable
                (RawInline _ (thead -> Just ':')):_             -> unshortcutable
                (RawInline _ (T.stripPrefix " [" -> Just _ )):_ -> unshortcutable
                _                                               -> shortcutable
            _ -> shortcutable
          where shortcutable = liftM2 (<>) (inlineToTiddlyWiki opts i) (go is)
                unshortcutable = do
                    iMark <- inlineToTiddlyWiki opts i
                    fmap (iMark <>) (go is)
                thead = fmap fst . T.uncons

isSp :: Inline -> Bool
isSp Space     = True
isSp SoftBreak = True
isSp _         = False

avoidBadWrapsInList :: [Inline] -> [Inline]
avoidBadWrapsInList [] = []
avoidBadWrapsInList (s:Str (T.uncons -> Just ('>',cs)):xs) | isSp s =
  Str (" >" <> cs) : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str (T.uncons -> Just (c, cs)):[])
  | T.null cs && isSp s && c `elem` ['-','*','+'] = Str (T.pack [' ', c]) : []
avoidBadWrapsInList (s:Str (T.uncons -> Just (c, cs)):Space:xs)
  | T.null cs && isSp s && c `elem` ['-','*','+'] =
    Str (T.pack [' ', c]) : Space : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str cs:Space:xs)
  | isSp s && isOrderedListMarker cs =
    Str (" " <> cs) : Space : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str cs:[])
  | isSp s && isOrderedListMarker cs = Str (" " <> cs) : []
avoidBadWrapsInList (x:xs) = x : avoidBadWrapsInList xs

isOrderedListMarker :: Text -> Bool
isOrderedListMarker xs = not (T.null xs) && (T.last xs `elem` ['.',')']) &&
              isRight (runParser (anyOrderedListMarker >> eof)
                       defaultParserState "" xs)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False

-- | Convert Pandoc inline element to tiddlywiki.
inlineToTiddlyWiki :: PandocMonad m => WriterOptions -> Inline -> MD m (Doc Text)
inlineToTiddlyWiki opts (Span attrs ils) = do
  contents <- inlineListToTiddlyWiki opts ils
  return $
    if attrs == nullAttr
    then contents
    else let attrs' = if attrs /= nullAttr
                      then styleAttrsToTiddlyWiki attrs <> space
                      else empty
         in "@@" <> attrs' <> contents <> "@@"
inlineToTiddlyWiki _ (Emph []) = return empty
inlineToTiddlyWiki opts (Emph lst) = do
  contents <- inlineListToTiddlyWiki opts lst
  return $ "//" <> contents <> "//"
inlineToTiddlyWiki _ (Underline []) = return empty
inlineToTiddlyWiki opts (Underline lst) = do
  contents <- inlineListToTiddlyWiki opts lst
  return $ "__" <> contents <> "__"
inlineToTiddlyWiki _ (Strong []) = return empty
inlineToTiddlyWiki opts (Strong lst) = do
  contents <- inlineListToTiddlyWiki opts lst
  return $ "''" <> contents <> "''"
inlineToTiddlyWiki _ (Strikeout []) = return empty
inlineToTiddlyWiki opts (Strikeout lst) = do
  contents <- inlineListToTiddlyWiki opts lst
  return $ "~~" <> contents <> "~~"
inlineToTiddlyWiki _ (Superscript []) = return empty
inlineToTiddlyWiki opts (Superscript lst) =
  local (\env -> env {envEscapeSpaces = True}) $ do
    contents <- inlineListToTiddlyWiki opts lst
    return $ "^^" <> contents <> "^^"
inlineToTiddlyWiki _ (Subscript []) = return empty
inlineToTiddlyWiki opts (Subscript lst) =
  local (\env -> env {envEscapeSpaces = True}) $ do
    contents <- inlineListToTiddlyWiki opts lst
    return $ ",," <> contents <> ",,"
inlineToTiddlyWiki opts (SmallCaps lst) = do
  if (isEnabled Ext_raw_html opts || isEnabled Ext_native_spans opts)
     then inlineToTiddlyWiki opts (Span ("",["smallcaps"],[]) lst)
     else inlineListToTiddlyWiki opts $ capitalize lst
inlineToTiddlyWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToTiddlyWiki opts lst
  return $ "‘" <> contents <> "’"
inlineToTiddlyWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToTiddlyWiki opts lst
  return $ "“" <> contents <> "”"
inlineToTiddlyWiki _ (Code _ str) = do
  return $ literal ("`" <> str <> "`")
inlineToTiddlyWiki opts (Str str) = do
  let str' = (escapeText opts) $ str
  return $ literal str'
inlineToTiddlyWiki opts (Math InlineMath str) =
  case writerHTMLMathMethod opts of
       WebTeX url -> inlineToTiddlyWiki opts
                       (Image nullAttr [Str str] (url <> T.pack (urlEncode $ T.unpack str), str))
       _ | isEnabled Ext_tex_math_dollars opts ->
             return $ "$$" <> literal str <> "$$"
         | otherwise -> do
             texMathToInlines InlineMath str >>=
               inlineListToTiddlyWiki opts . id
inlineToTiddlyWiki opts (Math DisplayMath str) =
  case writerHTMLMathMethod opts of
      WebTeX url -> (\x -> blankline <> x <> blankline) `fmap`
             inlineToTiddlyWiki opts (Image nullAttr [Str str]
                    (url <> T.pack (urlEncode $ T.unpack str), str))
      _ | isEnabled Ext_tex_math_dollars opts ->
            return $ "$$" <> literal str <> "$$"
        | otherwise -> (\x -> cr <> x <> cr) `fmap`
            (texMathToInlines DisplayMath str >>= inlineListToTiddlyWiki opts)
inlineToTiddlyWiki opts il@(RawInline f str) = do
  let tickGroups = filter (T.any (== '`')) $ T.group str
  let numticks = if null tickGroups
                 then 1
                 else 1 + maximum (map T.length tickGroups)
  let Format fmt = f
  render' fmt numticks
  where
    rawAttribInline fmt numticks  =
      return $ literal (T.replicate numticks "`") <> literal str <>
      literal (T.replicate numticks "`") <> literal "{=" <> literal fmt <> literal "}"
    renderEmpty = mempty <$ report (InlineNotRendered il)
    render' fmt numticks
      | f `elem` ["tiddlywiki"] =
          return $ literal str
      | isEnabled Ext_raw_attribute opts = rawAttribInline fmt numticks
      | f `elem` ["html", "html5", "html4"] =
            case () of
              _ | isEnabled Ext_raw_html opts -> return $ literal str
                | isEnabled Ext_raw_attribute opts -> rawAttribInline fmt numticks
                | otherwise -> renderEmpty
      | f `elem` ["latex", "tex"] =
            case () of
              _ | isEnabled Ext_raw_attribute opts -> rawAttribInline fmt numticks
                | otherwise -> renderEmpty
      | otherwise = renderEmpty
inlineToTiddlyWiki _ LineBreak = do
  return $ "  " <> cr
inlineToTiddlyWiki _ Space = do
  escapeSpaces <- asks envEscapeSpaces
  return $ if escapeSpaces then "\\ " else space
inlineToTiddlyWiki opts SoftBreak = do
  escapeSpaces <- asks envEscapeSpaces
  let space' = if escapeSpaces then "\\ " else space
  return $ case writerWrapText opts of
                WrapNone     -> space'
                WrapAuto     -> space'
                WrapPreserve -> cr
inlineToTiddlyWiki opts (Cite _ lst) = inlineListToTiddlyWiki opts lst
inlineToTiddlyWiki opts lnk@(Link attr txt (src, tit))
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    (literal . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain [lnk]])
  | otherwise = do
  linktext <- inlineListToTiddlyWiki opts txt
  let linktitle = if T.null tit
                     then empty
                     else literal $ " \"" <> tit <> "\""
  let srcSuffix = fromMaybe src (T.stripPrefix "mailto:" src)
  let useAuto = isURI src &&
                case txt of
                      [Str s] | escapeURI s == srcSuffix -> True
                      _       -> False
  let useRefLinks = writerReferenceLinks opts && not useAuto
  reftext <- if useRefLinks
                then literal <$> getReference attr linktext (src, tit)
                else return mempty
  return $ if useAuto
              then "<" <> literal srcSuffix <> ">"
              else if useRefLinks
                      then let first  = "[" <> linktext <> "]"
                               second = if getKey linktext == getKey reftext
                                           then "[]"
                                           else "[" <> reftext <> "]"
                           in  first <> second
                      else "[" <> linktext <> "](" <>
                           literal src <> linktitle <> ")" <>
                           linkAttributes opts attr
inlineToTiddlyWiki opts img@(Image attr alternate (source, tit))
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    (literal . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain [img]])
  | otherwise = do
  let txt = if null alternate || alternate == [Str source]
                                 -- to prevent autolinks
               then [Str ""]
               else alternate
  linkPart <- inlineToTiddlyWiki opts (Link attr txt (source, tit))
  return $ "!" <> linkPart
inlineToTiddlyWiki opts (Note contents) = do
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = literal $ writerIdentifierPrefix opts <> tshow (stNoteNum st + (length $ stNotes st) - 1)
  return $ "[" <> ref <> "]"

lineBreakToSpace :: Inline -> Inline
lineBreakToSpace LineBreak = Space
lineBreakToSpace SoftBreak = Space
lineBreakToSpace x         = x
