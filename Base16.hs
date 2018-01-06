{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Base16 where

import Data.List (tails)

import Data.Colour.CIE
import Data.Colour.CIE.Illuminant (d65)
import Data.Colour.SRGB

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Graphics.Rendering.Chart hiding (translate)
import Graphics.Rendering.Chart.Backend.Diagrams


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


contrastGrid :: [Colour Double] -> Diagram B
contrastGrid theme =
    mconcat rows
  where
    colored c = fillColor c . lineColor c
    rows =
      do
        (row, color1, color2s) <- zip3 [0..] theme (tails theme)
        let
          dia1 = colored color1 (square 2)
        [ translate (V2 (3 * row) (-3 * row)) (mconcat (cols dia1 color2s)) ]
    cols dia1 color2s =
      do
        (col, color2) <- zip [0..] color2s
        let dia2 = colored color2 (circle 0.5)
        [ translateX (3 * col) (dia2 <> dia1) ]


lightnessPlot :: [Colour Double] -> Layout Int Double
lightnessPlot theme =
    def
    & layout_title .~ "Perceived Lightness"
    & layout_plots .~ [ plotBars bars ]
  where
    bars =
      def
      & plot_bars_style .~ BarsStacked
      & plot_bars_item_styles .~ styles
      & plot_bars_values .~ values

    styles =
        map (\c -> (FillStyleSolid (opaque c), Just def)) theme

    values =
      do
        (n, color) <- zip [0..] theme
        let
          ys =
              replicate n 0
              ++ [ lightness d65 color ]
              ++ replicate (length theme - n - 1) 0
        [ (n, ys) ]
