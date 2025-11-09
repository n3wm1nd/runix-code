{-# LANGUAGE OverloadedStrings #-}

-- | Markdown to Brick widget rendering using Pandoc AST
--
-- This module converts markdown directly to Brick widgets by walking
-- the Pandoc AST, with support for inline formatting and proper word wrapping.
module UI.Rendering
  ( markdownToWidgets
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Readers.CommonMark (readCommonMark)
import Brick.Types (Widget, Context, getContext, availWidthL, attrL, imageL, emptyResult, ctxAttrMapL, Size(..))
import qualified Brick.Types
import Brick.Widgets.Core (txt, str, withAttr, (<+>), vBox, emptyWidget)
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

-- | Convert markdown text to a list of Brick widgets (one per block)
--
-- Parses markdown using CommonMark and converts the Pandoc AST to widgets.
-- Returns a list of widgets, one for each block-level element.
-- On parse errors, returns a single widget with the original text.
markdownToWidgets :: forall n. Text -> [Widget n]
markdownToWidgets text = case runPure (readCommonMark readerOpts text) of
  Right (Pandoc _ blocks) -> addSpacing $ map renderBlock blocks
  Left _err -> [txt text]  -- Fallback to plain text on error
  where
    -- Add empty widget spacing between blocks
    addSpacing [] = []
    addSpacing [x] = [x]
    addSpacing (x:xs) = x : emptyWidget : addSpacing xs

-- | Reader options for markdown parsing
readerOpts :: ReaderOptions
readerOpts = def
  { readerExtensions = pandocExtensions  -- Enable common markdown extensions
  }

-- | Render a Pandoc block element to a Brick widget
renderBlock :: forall n. Block -> Widget n
-- For paragraphs, we need to render with formatting while still wrapping
-- We'll use a horizontal box of inline widgets with proper text wrapping
renderBlock (Para inlines) = renderWrappedInlines inlines
renderBlock (Plain inlines) = renderWrappedInlines inlines
renderBlock (Header _level _attr inlines) =
  let headerText = renderInlinesToText inlines
      headerAttr = case _level of
                     1 -> header1Attr
                     2 -> header2Attr
                     3 -> header3Attr
                     _ -> header3Attr  -- Use header3 style for levels 4-6
  in withAttr headerAttr $ txt headerText
renderBlock (CodeBlock _attr code) =
  withAttr codeBlockAttr $ vBox $ map (txt . ("  " <>)) (T.lines code)
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
