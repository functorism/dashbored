{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}

module BlockText where

import Data.Foldable (fold)
import Data.Time (TimeOfDay (..))

newtype Line a = Line (a, a, a, a) deriving (Foldable)

newtype Col a = Col (a, a, a) deriving (Foldable)

newtype Glyph a = Glyph (Line a, Line a, Line a)

instance Semigroup (Line Bool) where
  (Line (l1, l2, l3, l4)) <> (Line (r1, r2, r3, r4)) = Line (l1 || r1, l2 || r2, l3 || r3, l4 || r4)

instance Semigroup (Glyph Bool) where
  (Glyph (l1, l2, l3)) <> (Glyph (r1, r2, r3)) = Glyph (l1 <> r1, l2 <> r2, l3 <> r3)

instance Bounded (Line Bool) where
  minBound = Line (False, False, False, False)
  maxBound = Line (True, True, True, True)

instance Bounded (Glyph Bool) where
  minBound = Glyph (minBound, minBound, minBound)
  maxBound = Glyph (maxBound, maxBound, maxBound)

instance Monoid a => Monoid (Col a) where
  mempty = Col (mempty, mempty, mempty)

instance Semigroup a => Semigroup (Col a) where
  (Col (l1, l2, l3)) <> (Col (r1, r2, r3)) =
    Col
      ( l1 <> r1,
        l2 <> r2,
        l3 <> r3
      )

glyphToCol :: (Monoid a, Semigroup a) => Glyph a -> Col a
glyphToCol (Glyph (l1, l2, l3)) = Col (fold l1, fold l2, fold l3)

colToString :: Col String -> String
colToString (Col (c1, c2, c3)) = unlines [c1, c2, c3]

colon :: Col String
colon =
  Col
    ( " ",
      "▀",
      "▀"
    )

space :: Col String
space =
  Col
    ( " ",
      " ",
      " "
    )

buildGlyph =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line ("█", " ", " ", "█"),
      Line ("█", "▄", "▄", "█")
    )

n0 :: Glyph String
n0 =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line ("█", " ", " ", "█"),
      Line ("█", "▄", "▄", "█")
    )

n1 :: Glyph String
n1 =
  Glyph
    ( Line (" ", "▄", "▄", " "),
      Line (" ", " ", "█", " "),
      Line (" ", " ", "█", " ")
    )

n2 :: Glyph String
n2 =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line ("▄", "▄", "▄", "█"),
      Line ("█", "▄", "▄", "▄")
    )

n3 :: Glyph String
n3 =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line ("▄", "▄", "▄", "█"),
      Line ("▄", "▄", "▄", "█")
    )

n4 :: Glyph String
n4 =
  Glyph
    ( Line ("▄", " ", " ", "▄"),
      Line ("█", "▄", "▄", "█"),
      Line (" ", " ", " ", "█")
    )

n5 :: Glyph String
n5 =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line ("█", "▄", "▄", "▄"),
      Line ("▄", "▄", "▄", "█")
    )

n6 :: Glyph String
n6 =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line ("█", "▄", "▄", "▄"),
      Line ("█", "▄", "▄", "█")
    )

n7 :: Glyph String
n7 =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line (" ", " ", " ", "█"),
      Line (" ", " ", " ", "█")
    )

n8 :: Glyph String
n8 =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line ("█", "▄", "▄", "█"),
      Line ("█", "▄", "▄", "█")
    )

n9 :: Glyph String
n9 =
  Glyph
    ( Line ("▄", "▄", "▄", "▄"),
      Line ("█", "▄", "▄", "█"),
      Line ("▄", "▄", "▄", "█")
    )

missing :: Glyph String
missing =
  Glyph
    ( Line ("M", "I", "S", "S"),
      Line ("M", "I", "S", "S"),
      Line ("M", "I", "S", "S")
    )

intToCol :: Int -> Col String
intToCol 0 = glyphToCol n0 <> space <> glyphToCol n0 <> space
intToCol n =
  (if n < 10 then glyphToCol n0 <> space else mempty)
    <> fold ((<> space) . digitToCol <$> digits n)

digitToCol :: Int -> Col String
digitToCol 0 = glyphToCol n0
digitToCol 1 = glyphToCol n1
digitToCol 2 = glyphToCol n2
digitToCol 3 = glyphToCol n3
digitToCol 4 = glyphToCol n4
digitToCol 5 = glyphToCol n5
digitToCol 6 = glyphToCol n6
digitToCol 7 = glyphToCol n7
digitToCol 8 = glyphToCol n8
digitToCol 9 = glyphToCol n9
digitToCol _ = glyphToCol missing

digits :: Integral a => a -> [a]
digits m =
  let go n ns = if n > 0 then go (div n 10) (mod n 10 : ns) else ns
   in go m []

todToBlockText :: TimeOfDay -> String
todToBlockText tod =
  colToString $
    intToCol (todHour tod)
      <> colon
      <> space
      <> intToCol (todMin tod)
      <> ticker (show (truncate $ todSec tod :: Int))

ticker :: String -> Col String
ticker t = Col (t, "", "")
