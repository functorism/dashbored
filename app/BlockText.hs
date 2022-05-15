{-# LANGUAGE FlexibleInstances #-}

module BlockText where

import Data.Foldable (fold)
import Data.Matrix (Matrix, flatten, fromList, fromLists, matrix, toLists, (<|>))
import Data.Time (TimeOfDay (..))

topChar :: Char
topChar = '▄'

bottomChar :: Char
bottomChar = '▄'

fillChar :: Char
fillChar = '█'

emptyChar :: Char
emptyChar = ' '

o :: Char
o = emptyChar

f :: Char
f = fillChar

t :: Char
t = topChar

b :: Char
b = bottomChar

colon :: Matrix Char
colon = fromLists [[emptyChar], [topChar], [topChar]]

space :: Matrix Char
space = matrix 3 1 (const emptyChar)

none :: Matrix Char
none = matrix 3 0 (const emptyChar)

n0 :: Matrix Char
n0 =
  fromLists
    [ [b, b, b, b],
      [f, o, o, f],
      [f, b, b, f]
    ]

n1 :: Matrix Char
n1 =
  fromLists
    [ [o, b, b, o],
      [o, o, f, o],
      [o, o, f, o]
    ]

n2 :: Matrix Char
n2 =
  fromLists
    [ [b, b, b, b],
      [b, b, b, f],
      [f, b, b, b]
    ]

n3 :: Matrix Char
n3 =
  fromLists
    [ [b, b, b, b],
      [b, b, b, f],
      [b, b, b, f]
    ]

n4 :: Matrix Char
n4 =
  fromLists
    [ [b, o, o, b],
      [f, b, b, f],
      [o, o, o, f]
    ]

n5 :: Matrix Char
n5 =
  fromLists
    [ [b, b, b, b],
      [f, b, b, b],
      [b, b, b, f]
    ]

n6 :: Matrix Char
n6 =
  fromLists
    [ [b, b, b, b],
      [f, b, b, b],
      [f, b, b, f]
    ]

n7 :: Matrix Char
n7 =
  fromLists
    [ [b, b, b, b],
      [o, o, o, f],
      [o, o, o, f]
    ]

n8 :: Matrix Char
n8 =
  fromLists
    [ [b, b, b, b],
      [f, b, b, f],
      [f, b, b, f]
    ]

n9 :: Matrix Char
n9 =
  fromLists
    [ [b, b, b, b],
      [f, b, b, f],
      [b, b, b, f]
    ]

missing :: Matrix Char
missing =
  fromLists
    [ ['M', 'I', 'S', 'S'],
      ['C', 'H', 'A', 'R'],
      ['N', 'U', 'L', 'L']
    ]

digitToCol :: Int -> Matrix Char
digitToCol 0 = n0
digitToCol 1 = n1
digitToCol 2 = n2
digitToCol 3 = n3
digitToCol 4 = n4
digitToCol 5 = n5
digitToCol 6 = n6
digitToCol 7 = n7
digitToCol 8 = n8
digitToCol 9 = n9
digitToCol _ = missing

intToCol :: Int -> Matrix Char
intToCol 0 = n0 <|> space <|> n0 <|> space
intToCol n =
  (if n < 10 then n0 <|> space else none)
    <|> foldr (<|>) none ((<|> space) . digitToCol <$> digits n)

digits :: Integral a => a -> [a]
digits m =
  let go n ns = if n > 0 then go (div n 10) (mod n 10 : ns) else ns
   in go m []

matToString :: Matrix Char -> String
matToString = foldMap (<> "\n") . toLists

todToBlockText :: TimeOfDay -> String
todToBlockText tod =
  matToString $
    intToCol (todHour tod)
      <|> colon
      <|> space
      <|> intToCol (todMin tod)
      <|> ticker (show (truncate $ todSec tod :: Int))

ticker :: String -> Matrix Char
ticker [c1, c2] = fromLists [[c1, c2], [' ', ' '], [' ', ' ']]
ticker [c1] = fromLists [['0', c1], [' ', ' '], [' ', ' ']]
ticker _ = none
