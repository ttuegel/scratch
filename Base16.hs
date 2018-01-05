{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Base16 where

import Data.Colour.CIE
import Data.Colour.CIE.Illuminant (d65)
import Data.Colour.SRGB

import Diagrams.Prelude
import Diagrams.Backend.SVG


chalk :: [Colour Double]
chalk =
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
    (zipWith color chalk (repeat (circle 1)))
  where
    color c = fillColor c . lineColor c
