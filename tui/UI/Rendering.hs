{-# LANGUAGE OverloadedStrings #-}

-- | Markdown to Brick widget rendering using Pandoc AST
--
-- This module converts markdown directly to Brick widgets by walking
-- the Pandoc AST, with support for inline formatting and proper word wrapping.
module UI.Rendering
  ( markdownToWidgets
  , markdownToWidgetsWithIndent
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc
import Brick.Types (Widget, Context, getContext, availWidthL, attrL, imageL, emptyResult, ctxAttrMapL, Size(..))
import qualified Brick.Types
import Brick.Widgets.Core (txt, withAttr, (<+>), vBox, padLeft, hBox)
import Brick.Widgets.Core (Padding(..))
import Skylighting (tokenize, TokenizerConfig(..), defaultSyntaxMap, lookupSyntax)
import Skylighting.Types (Token, TokenType(..), SourceLine)
import qualified Graphics.Vty as V
import Brick.AttrMap (attrMapLookup, AttrName)
import Lens.Micro ((^.), (&), (.~))
import UI.Attributes (header1Attr, header2Attr, header3Attr, boldAttr, italicAttr,
                      underlineAttr, codeAttr, linkAttr, strikethroughAttr, quotedAttr)

-- | A text segment with an attribute
data TextSegment = TextSegment
  { segmentAttr :: Maybe AttrName  -- Nothing means default attr
  , segmentText :: Text
  }

-- | Convert markdown text to a list of Brick widgets with hierarchical section structure
--
-- Uses no base indentation (starts at column 0).
markdownToWidgets :: forall n. Text -> [Widget n]
markdownToWidgets = markdownToWidgetsWithIndent 0

-- | Convert markdown text to widgets with a base indentation offset
--
-- Parses markdown using CommonMark and renders with hierarchical indentation:
-- - Base indent: specified offset (e.g., 1 for message content)
-- - H1 sections: base + 0 spaces
-- - H2 sections: base + 2 spaces
-- - H3 sections: base + 4 spaces
-- - H4-H6 sections: base + 6 spaces
--
-- On parse errors, returns a single widget with the original text.
markdownToWidgetsWithIndent :: forall n. Int -> Text -> [Widget n]
markdownToWidgetsWithIndent baseIndent text = case runPure (readCommonMark readerOpts text) of
  Right (Pandoc _ blocks) -> renderBlocksHierarchical baseIndent blocks
  Left _err -> [padLeft (Pad baseIndent) (txt text)]  -- Fallback to plain text on error

-- | Reader options for markdown parsing
readerOpts :: ReaderOptions
readerOpts = def
  { readerExtensions = pandocExtensions  -- Enable common markdown extensions
  }

-- | Render blocks with hierarchical section structure
--
-- Headers create sections, and all content following a header is indented
-- to show it belongs to that section. Nested headers create nested indentation.
--
-- Indentation scheme (relative to base indent):
-- - H1: base + 0 spaces
-- - H2: base + 2 spaces
-- - H3: base + 4 spaces
-- - H4-H6: base + 6 spaces
renderBlocksHierarchical :: forall n. Int -> [Block] -> [Widget n]
renderBlocksHierarchical baseIndent blocks = renderSections baseIndent 0 blocks
  where
    -- Render a list of blocks at a given nesting level
    renderSections :: Int -> Int -> [Block] -> [Widget n]
    renderSections _base _level [] = []
    renderSections base level (Header hLevel _attr inlines : rest) =
      -- Found a header - create a section
      let (sectionBlocks, afterSection) = span (not . isHeaderAtOrAbove hLevel) rest
          headerWidget = renderHeaderAtLevel hLevel inlines
          -- Content within this section gets rendered at the header's indent level
          -- (not nested deeper - we want absolute positioning)
          contentWidgets = renderSections base hLevel sectionBlocks
          -- Use absolute indentation based on header level + base
          indent = base + indentForLevel hLevel
          -- Add spacing: blank line before header (if not first), header, blank line after
          blankLine = padLeft (Pad indent) (txt " ")
          headerWithSpacing = if level == 0 && hLevel == 1
                                then [padLeft (Pad indent) headerWidget, blankLine]  -- First H1, no space before
                                else [blankLine, padLeft (Pad indent) headerWidget, blankLine]
          sectionWidget = vBox (headerWithSpacing ++ contentWidgets)
      -- Continue rendering remaining blocks at the current level
      in sectionWidget : renderSections base level afterSection
    renderSections base level (block : rest) =
      -- Non-header block at current level
      let indent = base + indentForLevel level
          widget = padLeft (Pad indent) (renderBlock block)
          -- Add spacing between blocks (except between consecutive list items)
          widgets = case (block, rest) of
                      -- No extra spacing between list items
                      (BulletList _, BulletList _ : _) -> [widget]
                      (OrderedList _ _, OrderedList _ _ : _) -> [widget]
                      -- Add blank line after other blocks if there's more content
                      (_, _:_) -> [widget, padLeft (Pad indent) (txt " ")]
                      -- Last block, no spacing needed
                      (_, []) -> [widget]
      in widgets ++ renderSections base level rest

    -- Calculate relative indentation for a given header level
    -- Level 0 (root/no header) = 0
    -- H1=0, H2=2, H3=4, H4+=6
    indentForLevel :: Int -> Int
    indentForLevel 0 = 0  -- Root level / no header
    indentForLevel 1 = 0  -- H1
    indentForLevel 2 = 2  -- H2
    indentForLevel 3 = 4  -- H3
    indentForLevel _ = 6  -- H4-H6

    -- Check if a block is a header at or above the given level
    isHeaderAtOrAbove :: Int -> Block -> Bool
    isHeaderAtOrAbove targetLevel (Header level _ _) = level <= targetLevel
    isHeaderAtOrAbove _ _ = False

    -- Render a header with appropriate styling based on level
    renderHeaderAtLevel :: Int -> [Inline] -> Widget n
    renderHeaderAtLevel level inlines = renderHeader level inlines

-- | Render a header with appropriate styling based on level
renderHeader :: forall n. Int -> [Inline] -> Widget n
renderHeader level inlines =
  let headerText = renderInlinesToText inlines
      headerAttr = case level of
                     1 -> header1Attr
                     2 -> header2Attr
                     3 -> header3Attr
                     _ -> header3Attr  -- Use header3 style for levels 4-6
  in withAttr headerAttr $ txt headerText

-- | Render a Pandoc block element to a Brick widget
renderBlock :: forall n. Block -> Widget n
-- For paragraphs, we need to render with formatting while still wrapping
-- We'll use a horizontal box of inline widgets with proper text wrapping
renderBlock (Para inlines) = renderWrappedInlines inlines
renderBlock (Plain inlines) = renderWrappedInlines inlines
-- Headers are handled by renderBlocksHierarchical, but we need a fallback case
renderBlock (Header level _attr inlines) = renderHeader level inlines
renderBlock (CodeBlock attr code) = renderCodeBlock attr code
renderBlock (BlockQuote blocks) =
  vBox $ map (\b -> txt "> " <+> renderBlock b) blocks
renderBlock (BulletList items) =
  vBox [txt "• " <+> vBox (map renderBlock item) | item <- items]
renderBlock (OrderedList _attrs items) =
  vBox [txt (T.pack (show n ++ ". ")) <+> vBox (map renderBlock item)
       | (n, item) <- zip [1::Int ..] items]
renderBlock HorizontalRule = txt "---"
renderBlock (Div _attr blocks) = vBox (map renderBlock blocks)
renderBlock (Table _attr _caption _colSpecs (TableHead _headAttr headRows) tableBodies _tableFoot) =
  renderTable headRows tableBodies
renderBlock LineBlock{} = txt ""
renderBlock RawBlock{} = txt ""
renderBlock _ = txt ""

-- | Render a table with fixed-width columns
-- Calculates column widths based on content, then renders with proper alignment
renderTable :: forall n. [Row] -> [TableBody] -> Widget n
renderTable headRows tableBodies =
  let allRows = headRows ++ concatMap getBodyRows tableBodies
      -- Calculate column widths from all rows
      columnWidths = calculateColumnWidths allRows
      -- Render header
      headerWidgets = map (renderRow columnWidths) headRows
      -- Create separator line
      separator = renderSeparator columnWidths
      -- Render body rows
      bodyWidgets = concatMap (renderTableBodyWithWidths columnWidths) tableBodies
  in vBox $ headerWidgets ++ [separator] ++ bodyWidgets
  where
    getBodyRows :: TableBody -> [Row]
    getBodyRows (TableBody _attr _rowHeadCols _headRows bodyRows) = bodyRows

    -- Calculate the width needed for each column
    calculateColumnWidths :: [Row] -> [Int]
    calculateColumnWidths rows =
      let cellContents = map getRowCells rows
          numCols = maximum (0 : map length cellContents)
          -- Transpose to get columns
          columns = transpose' numCols cellContents
      in map maxColumnWidth columns

    getRowCells :: Row -> [Text]
    getRowCells (Row _attr cells) = map getCellText cells

    getCellText :: Cell -> Text
    getCellText (Cell _attr _align _rowSpan _colSpan blocks) =
      renderBlocksToText blocks

    -- Simple transpose that pads with empty strings
    transpose' :: Int -> [[Text]] -> [[Text]]
    transpose' numCols rows =
      [[if i < length row then row !! i else "" | row <- rows] | i <- [0..numCols-1]]

    maxColumnWidth :: [Text] -> Int
    maxColumnWidth texts = maximum (3 : map T.length texts)  -- Minimum width of 3

    renderSeparator :: [Int] -> Widget n
    renderSeparator widths =
      let segments = map (\w -> T.replicate (w + 2) "-") widths  -- +2 for padding
      in txt $ "|" <> T.intercalate "|" segments <> "|"

    renderTableBodyWithWidths :: [Int] -> TableBody -> [Widget n]
    renderTableBodyWithWidths widths (TableBody _attr _rowHeadCols _headRows bodyRows) =
      map (renderRow widths) bodyRows

    renderRow :: [Int] -> Row -> Widget n
    renderRow widths (Row _attr cells) =
      let cellWidgets = zipWith renderCellWithWidth widths cells
          -- Pad with empty cells if row has fewer cells than columns
          paddedWidgets = cellWidgets ++ replicate (length widths - length cellWidgets) (txt "|  ")
      in txt "|" <+> hBox paddedWidgets

    renderCellWithWidth :: Int -> Cell -> Widget n
    renderCellWithWidth width (Cell _attr _align _rowSpan _colSpan blocks) =
      let cellText = renderBlocksToText blocks
          padded = cellText <> T.replicate (width - T.length cellText) " "
      in txt (" " <> padded <> " |")

    -- Convert blocks to plain text for width calculation
    renderBlocksToText :: [Block] -> Text
    renderBlocksToText blocks = T.intercalate " " (map renderBlockToText blocks)

    renderBlockToText :: Block -> Text
    renderBlockToText (Para inlines) = renderInlinesToText inlines
    renderBlockToText (Plain inlines) = renderInlinesToText inlines
    renderBlockToText _ = ""

-- | Render inline elements with formatting and proper wrapping
-- This converts inlines to text segments with attributes, then wraps them
renderWrappedInlines :: forall n. [Inline] -> Widget n
renderWrappedInlines inlines =
  let segments = inlinesToSegments Nothing inlines
  in renderWrappedSegments segments

-- | Convert inline elements to text segments with attributes
inlinesToSegments :: Maybe AttrName -> [Inline] -> [TextSegment]
inlinesToSegments parentAttr = concatMap (inlineToSegments parentAttr)

-- | Convert a single inline element to text segments
inlineToSegments :: Maybe AttrName -> Inline -> [TextSegment]
inlineToSegments parentAttr (Str text) = [TextSegment parentAttr text]
inlineToSegments _parentAttr (Emph inlines) = inlinesToSegments (Just italicAttr) inlines
inlineToSegments _parentAttr (Underline inlines) = inlinesToSegments (Just underlineAttr) inlines
inlineToSegments _parentAttr (Strong inlines) = inlinesToSegments (Just boldAttr) inlines
inlineToSegments _parentAttr (Strikeout inlines) = inlinesToSegments (Just strikethroughAttr) inlines
inlineToSegments _parentAttr (Code _attr code) = [TextSegment (Just codeAttr) code]
inlineToSegments parentAttr Space = [TextSegment parentAttr " "]
inlineToSegments parentAttr SoftBreak = [TextSegment parentAttr " "]
inlineToSegments parentAttr LineBreak = [TextSegment parentAttr "\n"]
inlineToSegments _parentAttr (Link _attr inlines (_url, _title)) =
  inlinesToSegments (Just linkAttr) inlines
inlineToSegments parentAttr (Image _attr _inlines (_url, _title)) =
  [TextSegment parentAttr "[image]"]
inlineToSegments _parentAttr (Quoted _quoteType inlines) =
  TextSegment (Just quotedAttr) "\"" : inlinesToSegments (Just quotedAttr) inlines ++ [TextSegment (Just quotedAttr) "\""]
inlineToSegments parentAttr (Cite _citations inlines) = inlinesToSegments parentAttr inlines
inlineToSegments parentAttr (Span _attr inlines) = inlinesToSegments parentAttr inlines
inlineToSegments parentAttr (Math _mathType text) = [TextSegment parentAttr text]
inlineToSegments _parentAttr (RawInline _format _text) = []
inlineToSegments parentAttr (Superscript inlines) =
  TextSegment parentAttr "^" : inlinesToSegments parentAttr inlines
inlineToSegments parentAttr (Subscript inlines) =
  TextSegment parentAttr "_" : inlinesToSegments parentAttr inlines
inlineToSegments parentAttr (SmallCaps inlines) = inlinesToSegments parentAttr inlines
inlineToSegments _parentAttr Note{} = []

-- | Render text segments with word wrapping and attributes
-- This builds a widget that wraps at available width while preserving attributes
renderWrappedSegments :: forall n. [TextSegment] -> Widget n
renderWrappedSegments segments = Brick.Types.Widget Greedy Fixed $ do
  ctx <- getContext
  let availWidth = ctx ^. availWidthL
      wrappedLines = wrapSegments availWidth segments
      lineImages = map (renderSegmentLine ctx) wrappedLines
  return $ emptyResult & imageL .~ V.vertCat lineImages
  where
    renderSegmentLine :: Context n -> [TextSegment] -> V.Image
    renderSegmentLine ctx lineSegs =
      let segImages = map (renderSegment ctx) lineSegs
      in V.horizCat segImages

    renderSegment :: Context n -> TextSegment -> V.Image
    renderSegment ctx seg =
      let baseAttr = ctx ^. attrL
          attr' = case segmentAttr seg of
                    Nothing -> baseAttr
                    Just attrName -> attrMapLookup attrName (ctx ^. ctxAttrMapL)
      in V.text' attr' (segmentText seg)

-- | Wrap text segments at the given width
-- Simple word-wrapping algorithm that breaks on spaces
wrapSegments :: Int -> [TextSegment] -> [[TextSegment]]
wrapSegments maxWidth segments =
  let words' = segmentsToWords segments
  in wrapWords maxWidth words'
  where
    -- Convert segments to words (breaking on spaces)
    -- Special handling: don't split quoted content into separate words
    segmentsToWords :: [TextSegment] -> [[TextSegment]]
    segmentsToWords [] = []
    segmentsToWords segs =
      let grouped = groupQuotedSegments segs
      in concatMap segmentGroupToWords grouped

    -- Group segments so quoted content stays together
    groupQuotedSegments :: [TextSegment] -> [[TextSegment]]
    groupQuotedSegments [] = []
    groupQuotedSegments (seg:segs)
      | segmentAttr seg == Just quotedAttr =
          let (quotedGroup, rest) = span (\s -> segmentAttr s == Just quotedAttr) (seg:segs)
          in [quotedGroup] ++ groupQuotedSegments rest
      | otherwise =
          let (nonQuotedGroup, rest) = span (\s -> segmentAttr s /= Just quotedAttr) (seg:segs)
          in [nonQuotedGroup] ++ groupQuotedSegments rest

    -- Convert a group of segments to words
    -- IMPORTANT: Each segment must preserve its own attribute when split into words
    segmentGroupToWords :: [TextSegment] -> [[TextSegment]]
    segmentGroupToWords [] = []
    segmentGroupToWords segs
      | all (\s -> segmentAttr s == Just quotedAttr) segs = [segs]  -- Keep quoted segments together
      | otherwise =
          -- Split each segment individually, preserving its attribute
          let splitSegment seg =
                let ws = T.words (segmentText seg)
                in map (\w -> [TextSegment (segmentAttr seg) w]) ws
          in concatMap splitSegment segs

    -- Wrap words into lines
    wrapWords :: Int -> [[TextSegment]] -> [[TextSegment]]
    wrapWords _ [] = []
    wrapWords w (word:rest) =
      let (line, remaining) = fillLine w word rest
      in line : wrapWords w remaining

    -- Fill a line with as many words as fit
    fillLine :: Int -> [TextSegment] -> [[TextSegment]] -> ([TextSegment], [[TextSegment]])
    fillLine _ firstWord [] = (firstWord, [])
    fillLine w firstWord (nextWord:rest) =
      let currentLen = sum $ map (T.length . segmentText) firstWord
          nextLen = sum $ map (T.length . segmentText) nextWord
          withSpace = currentLen + 1 + nextLen  -- +1 for space between words
      in if withSpace <= w
         then let (line, remaining) = fillLine w (firstWord ++ [TextSegment Nothing " "] ++ nextWord) rest
              in (line, remaining)
         else (firstWord, nextWord:rest)

-- | Convert inline elements to plain text (for headers and fallback rendering)
renderInlinesToText :: [Inline] -> Text
renderInlinesToText = T.concat . map renderInlineToText
  where
    renderInlineToText (Str text) = text
    renderInlineToText (Emph inlines) = renderInlinesToText inlines
    renderInlineToText (Underline inlines) = renderInlinesToText inlines
    renderInlineToText (Strong inlines) = renderInlinesToText inlines
    renderInlineToText (Strikeout inlines) = renderInlinesToText inlines
    renderInlineToText (Code _attr code) = code
    renderInlineToText Space = " "
    renderInlineToText SoftBreak = " "
    renderInlineToText LineBreak = "\n"
    renderInlineToText (Link _attr inlines (_url, _title)) = renderInlinesToText inlines
    renderInlineToText (Image _attr _inlines (_url, _title)) = "[image]"
    renderInlineToText (Quoted _quoteType inlines) = "\"" <> renderInlinesToText inlines <> "\""
    renderInlineToText (Cite _citations inlines) = renderInlinesToText inlines
    renderInlineToText (Span _attr inlines) = renderInlinesToText inlines
    renderInlineToText (Math _mathType text) = text
    renderInlineToText (RawInline _format _text) = ""
    renderInlineToText (Superscript inlines) = "^" <> renderInlinesToText inlines
    renderInlineToText (Subscript inlines) = "_" <> renderInlinesToText inlines
    renderInlineToText (SmallCaps inlines) = renderInlinesToText inlines
    renderInlineToText Note{} = ""

-- | Render a code block with syntax highlighting
renderCodeBlock :: forall n. Attr -> Text -> Widget n
renderCodeBlock (_, classes, _) code =
  let lang = case classes of
               (l:_) -> l
               [] -> ""
      header = if T.null lang
                 then txt "┌─ code"
                 else txt $ "┌─ " <> lang
      footer = txt "└─"
  in vBox $
       [header] ++
       renderHighlightedCode lang code ++
       [footer]

-- | Render code with syntax highlighting
renderHighlightedCode :: forall n. Text -> Text -> [Widget n]
renderHighlightedCode lang code =
  case lookupSyntax lang defaultSyntaxMap of
    Nothing -> renderPlainCode code  -- No syntax definition, render plain
    Just syntax ->
      case tokenize (TokenizerConfig defaultSyntaxMap False) syntax code of
        Left _err -> renderPlainCode code  -- Tokenization failed, render plain
        Right sourceLines -> map renderSourceLine sourceLines
  where
    renderPlainCode :: Text -> [Widget n]
    renderPlainCode c = map (txt . ("│ " <>)) (T.lines c)

-- | Render a single source line with syntax highlighting
renderSourceLine :: forall n. SourceLine -> Widget n
renderSourceLine tokens =
  txt "│ " <+> hBox (map renderToken tokens)

-- | Render a single token with appropriate color
renderToken :: forall n. Token -> Widget n
renderToken (tokenType, tokenText) =
  let color = tokenTypeToColor tokenType
      attr = V.defAttr `V.withForeColor` color
  in Brick.Types.Widget Brick.Types.Fixed Brick.Types.Fixed $ do
       return $ emptyResult & imageL .~ V.text' attr tokenText

-- | Map Skylighting token types to Vty colors
tokenTypeToColor :: TokenType -> V.Color
tokenTypeToColor KeywordTok = V.cyan
tokenTypeToColor DataTypeTok = V.green
tokenTypeToColor DecValTok = V.magenta
tokenTypeToColor BaseNTok = V.magenta
tokenTypeToColor FloatTok = V.magenta
tokenTypeToColor ConstantTok = V.magenta
tokenTypeToColor CharTok = V.yellow
tokenTypeToColor StringTok = V.yellow
tokenTypeToColor CommentTok = V.brightBlack
tokenTypeToColor OtherTok = V.white
tokenTypeToColor AlertTok = V.red
tokenTypeToColor FunctionTok = V.blue
tokenTypeToColor RegionMarkerTok = V.white
tokenTypeToColor ErrorTok = V.red
tokenTypeToColor BuiltInTok = V.cyan
tokenTypeToColor ExtensionTok = V.white
tokenTypeToColor PreprocessorTok = V.brightMagenta
tokenTypeToColor AttributeTok = V.white
tokenTypeToColor DocumentationTok = V.brightBlack
tokenTypeToColor AnnotationTok = V.white
tokenTypeToColor CommentVarTok = V.brightBlack
tokenTypeToColor VariableTok = V.white
tokenTypeToColor ControlFlowTok = V.cyan
tokenTypeToColor OperatorTok = V.white
tokenTypeToColor SpecialCharTok = V.yellow
tokenTypeToColor VerbatimStringTok = V.yellow
tokenTypeToColor SpecialStringTok = V.yellow
tokenTypeToColor ImportTok = V.cyan
tokenTypeToColor InformationTok = V.brightBlack
tokenTypeToColor WarningTok = V.yellow
tokenTypeToColor NormalTok = V.white
