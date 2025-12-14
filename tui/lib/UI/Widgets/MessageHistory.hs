{-# LANGUAGE RankNTypes #-}

-- | A scrollable message history widget with scroll indicators
--
-- This widget provides:
-- - Viewport-based scrolling for a list of variable-height widgets
-- - Overlay scroll indicators showing rows above/below
-- - Auto-scroll to bottom on content updates
-- - Stay-at-bottom behavior on resize
module UI.Widgets.MessageHistory
  ( -- * Widget
    messageHistory
    -- * Viewport Operations
  , isAtBottom
  ) where

import qualified Brick.Types as T
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import Lens.Micro

-- | Render a scrollable message history with scroll indicators
--
-- This widget creates a viewport and renders the provided widgets inside it,
-- with scroll indicators overlaid at the top-right and bottom-right corners.
--
-- **Returns a tuple for Brick's layer system**: The proper way to overlay in Brick
-- is using multiple widget layers rendered by Brick's `renderFinal`. This is because:
-- 1. Vty doesn't support true transparency - we can't build a "sparse" overlay image
-- 2. V.resize and V.charFill both overwrite the base content
-- 3. Brick's layer system is designed exactly for this use case
--
-- Usage: @drawUI st = [indicators, baseUI]@ where @(base, indicators) = messageHistory ...@
messageHistory :: (Ord n, Show n)
               => n                    -- ^ Viewport name
               -> Maybe T.Viewport     -- ^ Last known viewport state (for indicators)
               -> [T.Widget n]         -- ^ Widgets to display (oldest first)
               -> (T.Widget n, T.Widget n)  -- ^ (base viewport, indicator overlay for Brick layers)
messageHistory vpName mLastVp widgets =
    let baseWidget = viewport vpName T.Vertical (vBox widgets)
        indicatorWidget = renderIndicatorOverlay mLastVp
    in (baseWidget, indicatorWidget)

-- | Render scroll indicator overlay
--
-- Creates a Fixed-size widget with indicators positioned using translateX/Y.
-- Must be rendered as a separate Brick layer for proper overlay.
renderIndicatorOverlay :: Maybe T.Viewport -> T.Widget n
renderIndicatorOverlay mLastVp = T.Widget T.Greedy T.Greedy $ do
    ctx <- T.getContext

    case mLastVp of
        Nothing -> return T.emptyResult
        Just vp -> do
            let screenWidth = ctx ^. T.availWidthL
                screenHeight = ctx ^. T.availHeightL
                scrollTop = vp ^. T.vpTop
                visibleHeight = snd (vp ^. T.vpSize)
                totalContentHeight = snd (vp ^. T.vpContentSize)
                scrollBottom = scrollTop + visibleHeight

                rowsAbove = scrollTop
                rowsBelow = max 0 (totalContentHeight - scrollBottom)

                topText = "↑" ++ show rowsAbove ++ "↑"
                bottomText = "↓" ++ show rowsBelow ++ "↓"

            -- Create indicator images at absolute screen positions using translateX/Y
            -- Both indicators are positioned relative to the full screen dimensions
            let topImg = if rowsAbove > 0
                        then V.translateX (screenWidth - length topText) $
                             V.translateY 0 $
                             V.string V.defAttr topText
                        else V.emptyImage

                bottomImg = if rowsBelow > 0
                           then V.translateX (screenWidth - length bottomText) $
                                V.translateY (screenHeight - 1) $
                                V.string V.defAttr bottomText
                           else V.emptyImage

                -- Use horizJoin to overlay both indicators without stacking
                -- This works because they're at different Y positions due to translateY
                combinedImg = topImg `V.horizJoin` bottomImg

            return $ T.emptyResult & T.imageL .~ combinedImg

-- | Check if viewport is scrolled to the bottom
isAtBottom :: T.Viewport -> Bool
isAtBottom vp =
    let scrollTop = vp ^. T.vpTop
        visibleHeight = snd (vp ^. T.vpSize)
        totalContentHeight = snd (vp ^. T.vpContentSize)
        scrollBottom = scrollTop + visibleHeight
    in scrollBottom >= totalContentHeight
