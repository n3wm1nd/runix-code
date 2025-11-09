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
import Text.Pandoc.Readers.CommonMark (readCommonMark)
import Brick.Types (Widget, Context, getContext, availWidthL, attrL, imageL, emptyResult, ctxAttrMapL, Size(..))
import qualified Brick.Types
import Brick.Widgets.Core (txt, str, withAttr, (<+>), vBox, emptyWidget, padLeft, hBox)
import Brick.Widgets.Core (Padding(..))
import Skylighting (tokenize, TokenizerConfig(..), defaultSyntaxMap, lookupSyntax)
import Skylighting.Types (Token(..), TokenType(..), SourceLine)
import qualified Graphics.Vty as V
import Data.Maybe (fromMaybe)
import Brick.AttrMap (attrMapLookup, AttrName)
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (&), (.~))
import UI.Attributes (header1Attr, header2Attr, header3Attr, boldAttr, italicAttr,
                      underlineAttr, codeAttr, codeBlockAttr, linkAttr, strikethroughAttr)

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
          -- Don't wrap with padLeft - children handle their own indentation
          sectionWidget = vBox (padLeft (Pad indent) headerWidget : contentWidgets)
      -- Continue rendering remaining blocks at the current level
      in sectionWidget : renderSections base level afterSection
    renderSections base level (block : rest) =
      -- Non-header block at current level
      let indent = base + indentForLevel level
          widget = padLeft (Pad indent) (renderBlock block)
      in widget : renderSections base level rest

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
    renderHeaderAtLevel level inlines =
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
renderBlock (Header level _attr inlines) =
  let headerText = renderInlinesToText inlines
      headerAttr = case level of
                     1 -> header1Attr
                     2 -> header2Attr
                     3 -> header3Attr
                     _ -> header3Attr
  in withAttr headerAttr $ txt headerText
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
renderBlock LineBlock{} = txt ""
renderBlock RawBlock{} = txt ""
renderBlock _ = txt ""

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
inlineToSegments parentAttr (Quoted _quoteType inlines) =
  TextSegment parentAttr "\"" : inlinesToSegments parentAttr inlines ++ [TextSegment parentAttr "\""]
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
    segmentsToWords :: [TextSegment] -> [[TextSegment]]
    segmentsToWords [] = []
    segmentsToWords (seg:segs) =
      let segText = segmentText seg
          segAttr = segmentAttr seg
          ws = T.words segText
          -- Preserve the attribute for each word
          wordSegs = map (\w -> TextSegment segAttr w) ws
      in map (:[]) wordSegs ++ segmentsToWords segs

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
