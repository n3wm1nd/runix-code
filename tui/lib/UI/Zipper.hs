{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generic zipper data structures for efficient navigation and editing
--
-- This module provides two fundamental zipper types:
--
-- * 'Zipper' - NON-EMPTY zipper pointing to exactly one element (current focus)
--   Useful for: navigation through lists, focusing on specific items
--   Structure: back (reversed) <- CURRENT -> front
--   Invariant: Always contains at least one element
--
-- * 'GapZipper' - Points between two elements (gap/cursor position), can be empty
--   Useful for: text editing, insertion at a position
--   Structure: before (reversed) | GAP | after
--
-- Both types support common operations via the 'Zippable' typeclass.
module UI.Zipper
  ( -- * Non-empty zipper with focus on element
    Zipper(..)
  , singletonZipper
  , zipperToList
  , listToZipper
  , focusNewest
  , focusOldest
  , insertItem
  , updateCurrent
  , appendItem
  , appendItemAndFocus
  , getCurrent
  , hasNewer
  , hasOlder
  , mapZipper

    -- * Gap zipper (cursor between elements)
  , GapZipper(..)
  , emptyGap
  , gapToList
  , listToGap
  , gapAtStart
  , gapAtEnd
  , insertAtGap
  , deleteBeforeGap
  , deleteAfterGap
  , getBeforeGap
  , getAfterGap
  , mapGap
  , filterGap

    -- * Typeclass for common operations
  , Zippable(..)

    -- * Traversal helpers
  , onRewound
  , onForwardUntil
  ) where

--------------------------------------------------------------------------------
-- Zipper with focus on element
--------------------------------------------------------------------------------

-- | A non-empty zipper that focuses on exactly one element
-- Structure: back (newer) <- CURRENT -> front (older)
-- Invariant: Always has a current element (non-empty by construction)
data Zipper a = Zipper
  { zipperBack :: [a]     -- ^ Newer items (reverse chronological)
  , zipperCurrent :: a    -- ^ Focused item (always present)
  , zipperFront :: [a]    -- ^ Older items (chronological from current)
  } deriving stock (Eq, Show, Ord)

-- | Create a zipper with a single focused element
singletonZipper :: a -> Zipper a
singletonZipper x = Zipper [] x []

-- | Convert zipper to a list (newest first)
zipperToList :: Zipper a -> [a]
zipperToList (Zipper back current front) =
  reverse back ++ [current] ++ front

-- | Create a zipper from a non-empty list (newest first)
-- Focus will be on the newest item (head of list)
-- Returns Nothing if input list is empty
listToZipper :: [a] -> Maybe (Zipper a)
listToZipper [] = Nothing
listToZipper (newest:older) = Just (Zipper [] newest older)

-- | Focus on the newest item (O(1) operation)
focusNewest :: Zipper a -> Zipper a
focusNewest z@(Zipper [] _ _) = z  -- Already at newest
focusNewest (Zipper (b:bs) current front) =
  -- Move newest from back to current
  Zipper bs b (current : front)

-- | Focus on the oldest item (O(n) operation)
focusOldest :: Zipper a -> Zipper a
focusOldest z@(Zipper _ _ []) = z  -- Already at oldest
focusOldest (Zipper back current (f:fs)) =
  -- Move oldest from front to current
  Zipper (current : back) f fs

-- | Insert a new item at the newest position (prepend to back)
insertItem :: a -> Zipper a -> Zipper a
insertItem item (Zipper back current front) =
  Zipper (item:back) current front

-- | Update the current focused item
updateCurrent :: a -> Zipper a -> Zipper a
updateCurrent item (Zipper back _ front) =
  Zipper back item front

-- | Add a new item as current, moving old current to front
appendItem :: a -> Zipper a -> Zipper a
appendItem item (Zipper back current front) =
  Zipper back item (current : front)

-- | Add a new item as current, moving old current to back (keeps focus)
appendItemAndFocus :: a -> Zipper a -> Zipper a
appendItemAndFocus item (Zipper back current front) =
  Zipper (current : back) item front

-- | Get the currently focused item
getCurrent :: Zipper a -> a
getCurrent = zipperCurrent

-- | Check if there are newer items
hasNewer :: Zipper a -> Bool
hasNewer (Zipper back _ _) = not (null back)

-- | Check if there are older items
hasOlder :: Zipper a -> Bool
hasOlder (Zipper _ _ front) = not (null front)

-- | Map a function over all elements in the zipper
mapZipper :: (a -> b) -> Zipper a -> Zipper b
mapZipper f (Zipper back current front) =
  Zipper (map f back) (f current) (map f front)

--------------------------------------------------------------------------------
-- Gap zipper (cursor between elements)
--------------------------------------------------------------------------------

-- | A zipper that represents a gap/cursor position between elements
-- Structure: before (reversed) | GAP | after
-- The gap is where insertions happen
data GapZipper a = GapZipper
  { gapBefore :: [a]  -- ^ Elements before gap (reversed, head is closest to gap)
  , gapAfter :: [a]   -- ^ Elements after gap (normal order, head is closest to gap)
  } deriving stock (Eq, Show, Ord)

-- | Create an empty gap zipper (gap at start/end, no elements)
emptyGap :: GapZipper a
emptyGap = GapZipper [] []

-- | Convert gap zipper to a list
gapToList :: GapZipper a -> [a]
gapToList (GapZipper before after) = reverse before ++ after

-- | Create a gap zipper from a list (gap at end)
listToGap :: [a] -> GapZipper a
listToGap xs = GapZipper (reverse xs) []

-- | Check if gap is at the start
gapAtStart :: GapZipper a -> Bool
gapAtStart (GapZipper before _) = null before

-- | Check if gap is at the end
gapAtEnd :: GapZipper a -> Bool
gapAtEnd (GapZipper _ after) = null after

-- | Insert an element at the gap (goes before gap, gap moves right)
insertAtGap :: a -> GapZipper a -> GapZipper a
insertAtGap x (GapZipper before after) = GapZipper (x:before) after

-- | Delete element before the gap (backspace)
deleteBeforeGap :: GapZipper a -> GapZipper a
deleteBeforeGap (GapZipper [] after) = GapZipper [] after  -- Nothing to delete
deleteBeforeGap (GapZipper (_:before) after) = GapZipper before after

-- | Delete element after the gap (delete key)
deleteAfterGap :: GapZipper a -> GapZipper a
deleteAfterGap (GapZipper before []) = GapZipper before []  -- Nothing to delete
deleteAfterGap (GapZipper before (_:after)) = GapZipper before after

-- | Get element immediately before the gap
getBeforeGap :: GapZipper a -> Maybe a
getBeforeGap (GapZipper [] _) = Nothing
getBeforeGap (GapZipper (x:_) _) = Just x

-- | Get element immediately after the gap
getAfterGap :: GapZipper a -> Maybe a
getAfterGap (GapZipper _ []) = Nothing
getAfterGap (GapZipper _ (x:_)) = Just x

-- | Map a function over all elements in the gap zipper
mapGap :: (a -> b) -> GapZipper a -> GapZipper b
mapGap f (GapZipper before after) =
  GapZipper (map f before) (map f after)

-- | Filter gap zipper elements
filterGap :: (a -> Bool) -> GapZipper a -> GapZipper a
filterGap p (GapZipper before after) =
  GapZipper (filter p before) (filter p after)

--------------------------------------------------------------------------------
-- Typeclass for common operations
--------------------------------------------------------------------------------

-- | Common operations for zipper-like structures
class Zippable z where
  -- | Move focus/gap forward (toward newer/right)
  forward :: z a -> z a

  -- | Move focus/gap backward (toward older/left)
  back :: z a -> z a

  -- | Move to the start position (O(1) or better when overridden)
  start :: z a -> z a
  start z
    | atStart z = z
    | otherwise = start (back z)

  -- | Move to the end position (O(1) or better when overridden)
  end :: z a -> z a
  end z
    | atEnd z = z
    | otherwise = end (forward z)

  -- | Check if at the start
  atStart :: z a -> Bool

  -- | Check if at the end
  atEnd :: z a -> Bool

  -- | Convert to list
  toList :: z a -> [a]

-- | Zipper instance: forward moves to newer (back), back moves to older (front)
instance Zippable Zipper where
  forward z@(Zipper [] _ _) = z  -- No newer items
  forward (Zipper (b:bs) current front) =
    Zipper bs b (current : front)

  back z@(Zipper _ _ []) = z  -- No older items
  back (Zipper back current (f:fs)) =
    Zipper (current : back) f fs

  -- Efficient O(1) start: move all to front
  start (Zipper [] cur front) = Zipper [] cur front
  start (Zipper back cur front) =
    -- Move all back items to front in correct order
    Zipper [] (last back) (reverse (init back) ++ [cur] ++ front)

  -- Efficient O(1) end: move all to back
  end (Zipper back cur []) = Zipper back cur []
  end (Zipper back cur front) =
    -- Move all front items to back in correct order
    Zipper (reverse (init front) ++ [cur] ++ back) (last front) []

  atStart (Zipper back _ _) = null back
  atEnd (Zipper _ _ front) = null front

  toList = zipperToList

-- | GapZipper instance: forward moves gap right, back moves gap left
instance Zippable GapZipper where
  forward (GapZipper before []) = GapZipper before []  -- Already at end
  forward (GapZipper before (a:after)) = GapZipper (a:before) after

  back (GapZipper [] after) = GapZipper [] after  -- Already at start
  back (GapZipper (b:before) after) = GapZipper before (b:after)

  -- Efficient O(1) start: move all to after
  start (GapZipper [] after) = GapZipper [] after
  start (GapZipper before after) = GapZipper [] (reverse before ++ after)

  -- Efficient O(1) end: move all to before
  end (GapZipper before []) = GapZipper before []
  end (GapZipper before after) = GapZipper (reverse after ++ before) []

  atStart = gapAtStart
  atEnd = gapAtEnd

  toList = gapToList

--------------------------------------------------------------------------------
-- Traversal helpers
--------------------------------------------------------------------------------

-- | Run an operation on a rewound zipper, then restore focus
-- Recursively rewinds to oldest/start, applies operation, then moves forward same number of steps
onRewound :: Zippable z => (z a -> z a) -> z a -> z a
onRewound f zipper
  | atEnd zipper = f zipper  -- Already at oldest/end, apply operation
  | otherwise =
      let rewound = back zipper           -- Step back one
          modified = onRewound f rewound  -- Recurse (goes all the way back)
          restored = forward modified     -- Step forward one (as stack unwinds)
      in restored

-- | Move forward until condition is met, apply operation, then restore position
onForwardUntil :: Zippable z => (z a -> Bool) -> (z a -> z a) -> z a -> z a
onForwardUntil condition f zipper
  | condition zipper = f zipper  -- Condition met
  | atStart zipper = zipper      -- Can't move forward, give up
  | otherwise =
      let advanced = forward zipper
          modified = onForwardUntil condition f advanced
          restored = back modified
      in restored
