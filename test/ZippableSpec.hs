{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module ZippableSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import UI.Zipper
import UI.SegmentEditor
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

-- QuickCheck Arbitrary instances
instance Arbitrary a => Arbitrary (GapZipper a) where
  arbitrary = do
    before <- arbitrary
    after <- arbitrary
    return $ GapZipper before after

instance Arbitrary a => Arbitrary (Zipper a) where
  arbitrary = do
    back <- arbitrary
    current <- arbitrary
    front <- arbitrary
    return $ Zipper back current front

-- Arbitrary instance for InputSegment
instance Arbitrary InputSegment where
  arbitrary = oneof
    [ CharSegment <$> arbitrary
    , PastedSegment <$> (T.pack <$> arbitrary)
    ]

-- Arbitrary instance for SegmentEditor
-- Maintains invariants:
-- 1. Lines in edLinesAbove are reversed (head is last segment)
-- 2. Lines in edLinesBelow are forward (head is first segment)
-- 3. edCurrentLine can be empty only if both above/below are empty
instance Arbitrary (SegmentEditor () InputSegment) where
  arbitrary = do
    -- Generate arbitrary gap zippers for lines
    linesAbove <- arbitrary :: Gen [GapZipper InputSegment]
    currentLine <- arbitrary :: Gen (GapZipper InputSegment)
    linesBelow <- arbitrary :: Gen [GapZipper InputSegment]

    -- Convert to lists and apply ordering invariants
    let linesAboveNonEmpty = filter (not . null) (map toList linesAbove)
        linesBelowNonEmpty = filter (not . null) (map toList linesBelow)
        -- edLinesAbove stores reversed segments
        finalAboveReversed = map reverse linesAboveNonEmpty
        -- edLinesBelow stores forward segments
        finalBelowForward = linesBelowNonEmpty

        -- If current line is empty, clear above/below to maintain invariant
        (finalAbove, finalBelow) =
          if null (toList currentLine)
          then ([], [])
          else (finalAboveReversed, finalBelowForward)

        config = EditorConfig { editorName = (), lineLimit = Nothing, newlineMode = EnterSends }

    return $ SegmentEditor config finalAbove currentLine finalBelow

-- Generic Zippable laws that should hold for all instances
zippableLaws :: forall z a. (Zippable z, Arbitrary (z a), Arbitrary a, Show (z a), Eq (z a), Eq a, Show a, Ord a)
             => String -> Spec
zippableLaws name = describe ("Zippable Laws for " ++ name) $ do
  it "back . forward = id (when not at start)" $ property $
    \(z :: z a) ->
      not (atStart z) ==> forward (back z) == z

  it "forward . back = id (when not at end)" $ property $
    \(z :: z a) ->
      not (atEnd z) ==> back (forward z) == z

  it "start moves to start" $ property $
    \(z :: z a) -> atStart (start z)

  it "end moves to end" $ property $
    \(z :: z a) -> atEnd (end z)

  it "back from start stays at start" $ property $
    \(z :: z a) ->
      let z' = start z
      in back z' == z'

  it "forward from end stays at end" $ property $
    \(z :: z a) ->
      let z' = end z
      in forward z' == z'

  it "movement preserves data" $ property $
    \(z :: z a) ->
      let z' = forward (back (forward (start (end z))))
      in sort (toList z) == sort (toList z')

  it "insertBackward adds element to data" $ property $
    \(x :: a) (z :: z a) ->
      let z' = insertBackward x z
      in x `elem` toList z' && length (toList z') == length (toList z) + 1

  it "insertForward adds element to data" $ property $
    \(x :: a) (z :: z a) ->
      let z' = insertForward x z
      in x `elem` toList z' && length (toList z') == length (toList z) + 1

  it "deleteBackward removes element (when not at start)" $ property $
    \(z :: z a) ->
      not (atStart z) ==>
        case deleteBackward z of
          Nothing -> False  -- Should succeed when not at start
          Just z' -> length (toList z') == length (toList z) - 1

  it "deleteForward removes element (when not at end)" $ property $
    \(z :: z a) ->
      not (atEnd z) ==>
        case deleteForward z of
          Nothing -> False  -- Should succeed when not at end
          Just z' -> length (toList z') == length (toList z) - 1

  it "insertBackward then deleteBackward is noop" $ property $
    \(x :: a) (z :: z a) ->
      case deleteBackward (insertBackward x z) of
        Nothing -> False  -- Should always succeed after insert
        Just z' -> toList z' == toList z

  it "insertForward then deleteForward is noop" $ property $
    \(x :: a) (z :: z a) ->
      case deleteForward (insertForward x z) of
        Nothing -> False  -- Should always succeed after insert
        Just z' -> toList z' == toList z

spec :: Spec
spec = do
  zippableLaws @GapZipper @Int "GapZipper Int"
  zippableLaws @Zipper @Int "Zipper Int"
  zippableLaws @(SegmentEditor ()) @InputSegment "SegmentEditor"

  describe "Specific GapZipper tests" $ do
    it "toList preserves elements" $ property $
      \(xs :: [Int]) ->
        let z = listToGap xs
        in sort (toList z) == sort xs

    it "insertBackward adds element" $ property $
      \(x :: Int) (z :: GapZipper Int) ->
        let z' = insertBackward x z
        in x `elem` toList z'

    it "insertForward adds element" $ property $
      \(x :: Int) (z :: GapZipper Int) ->
        let z' = insertForward x z
        in x `elem` toList z'

    it "deleteBackward removes element (when not at start)" $ property $
      \(z :: GapZipper Int) ->
        not (atStart z) ==>
          case deleteBackward z of
            Nothing -> False
            Just z' -> length (toList z') == length (toList z) - 1

    it "deleteForward removes element (when not at end)" $ property $
      \(z :: GapZipper Int) ->
        not (atEnd z) ==>
          case deleteForward z of
            Nothing -> False
            Just z' -> length (toList z') == length (toList z) - 1

    it "deleteBackward at start returns Nothing" $ property $
      \(xs :: [Int]) ->
        let z = listToGap xs  -- Creates gap at end
            z' = start z       -- Move gap to start
        in deleteBackward z' == Nothing

  describe "Specific Zipper tests" $ do
    it "toList preserves elements" $ property $
      \(NonEmpty (xs :: [Int])) ->
        case listToZipper xs of
          Nothing -> False
          Just z -> sort (toList z) == sort xs

    it "insertBackward adds element" $ property $
      \(x :: Int) (z :: Zipper Int) ->
        let z' = insertBackward x z
        in x `elem` toList z'

    it "insertForward adds element" $ property $
      \(x :: Int) (z :: Zipper Int) ->
        let z' = insertForward x z
        in x `elem` toList z'

    it "deleteBackward removes element (when possible)" $ property $
      \(z :: Zipper Int) ->
        case deleteBackward z of
          Nothing -> null (zipperBack z)
          Just z' -> length (toList z') == length (toList z) - 1

    it "deleteForward removes element (when possible)" $ property $
      \(z :: Zipper Int) ->
        case deleteForward z of
          Nothing -> null (zipperFront z)
          Just z' -> length (toList z') == length (toList z) - 1
