{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Base16 where

import Data.List (tails)

import Data.Colour.CIE
import Data.Colour.CIE.Illuminant (d65)
import Data.Colour.SRGB

import Diagrams.Prelude
import Diagrams.Backend.SVG


chalk :: [Colour Double]
chalk =
    -- bad combinations:
    -- 0 + 1
    -- 1 + 2
    -- 5 + 6
    -- 8 + 9
    -- 9 + A
    -- C + D
    -- 9 + F
    -- A + F
    map sRGB24read
    [
      -- Black
      "#151515",

      -- Greys
      "#202020",
      "#303030",
      "#505050",
      "#b0b0b0",
      "#d0d0d0",
      "#e0e0e0",
      "#f5f5f5",

      -- White
      "#fb9fb1",

      -- Colors
      "#eda987",
      "#ddb26f",
      "#acc267",
      "#12cfc0",
      "#6fc2ef",
      "#e1a3ee",
      "#deaf8f"
    ]


contrastLine :: Diagram B
contrastLine =
    hcat' (with & catMethod .~ Distrib & sep .~ sqrt 2)
    (zipWith color chalk (repeat unitCircle))
  where
    color c = fillColor c . lineColor c


contrastGrid :: Diagram B
contrastGrid =
    mconcat contrastPairs
  where
    contrastPairs =
      do
        (col, color1, color2s) <- zip3 [0..] chalk (tails chalk)
        let
          colored c = fillColor c . lineColor c
          dia1 = translateX (-1 / sqrt 2) (colored color1 unitCircle)
        (row, color2) <- zip [0..] color2s
        let
          dia2 = translateX (1 / sqrt 2) (colored color2 unitCircle)
        [translate (V2 (4 * col) (4 * (col + row))) (dia1 <> dia2)]
