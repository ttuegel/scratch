{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Base16 where

import Data.Foldable
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Data.Colour.CIE
import Data.Colour.CIE.Illuminant (d65)
import Data.Colour.SRGB

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Graphics.Rendering.Chart hiding (translate)
import Graphics.Rendering.Chart.Backend.Diagrams


chalk :: NonEmpty (Colour Double)
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
    (<$>) sRGB24read
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
    (zipWith color (NonEmpty.toList chalk) (repeat unitCircle))
  where
    color c = fillColor c . lineColor c


contrastGrid :: [Colour Double] -> Diagram B
contrastGrid theme =
    mconcat rows
  where
    colored c = fillColor c . lineColor c
    rows =
      do
        (row, color1, color2s) <- zip3 [0..] theme (repeat theme)
        let
          dia1 = colored color1 (square 2)
        [ translate (V2 0 (-3 * row)) (mconcat (cols dia1 color2s)) ]
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
    & layout_x_axis.laxis_override .~ (axis_labels .~ xLabels)
  where
    bars =
      def
      & plot_bars_style .~ BarsStacked
      & plot_bars_item_styles .~ styles
      & plot_bars_values .~ values

    xLabels =
        [
          [
            (0, "00"),
            (1, "01"),
            (2, "02"),
            (3, "03"),
            (4, "04"),
            (5, "05"),
            (6, "06"),
            (7, "07"),
            (8, "08"),
            (9, "09"),
            (10, "0A"),
            (11, "0B"),
            (12, "0C"),
            (13, "0D"),
            (14, "0E"),
            (15, "0F")
          ]
        ]

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


idealLightnessRamp :: NonEmpty (Colour Double) -> Double
idealLightnessRamp (color00 :| colors) =
    (light07 - light00) / 7
  where
    light00 = lightness d65 color00
    light07 = lightness d65 (colors !! 6)


applyLightnessRamp
    :: Double -> NonEmpty (Colour Double) -> NonEmpty (Colour Double)
applyLightnessRamp ramp (color00 :| colors) =
    color00 :| (map toGray lightnesses ++ drop 6 colors)
  where
    (l00, a00, b00) = cieLABView d65 color00
    toGray :: Double -> Colour Double
    toGray l = cieLAB d65 l a00 b00
    lightnesses = (take 6 . drop 1) (iterate (+ ramp) l00)


chalk2 :: NonEmpty (Colour Double)
chalk2 = applyLightnessRamp (idealLightnessRamp chalk) chalk

plotLab :: Foldable f => f (Colour Double) -> Diagram B
plotLab colors =
    mconcat (plotLab1 <$> toList colors)
  where
    plotLab1 c =
        circle 0.05
        & translate (V2 a' b')
        & fillColor c
      where
        (_, a, b) = cieLABView d65 c
        a' = a / 100
        b' = b / 100


plotChroma :: NonEmpty (Colour Double) -> Diagram B
plotChroma (drop 8 . NonEmpty.toList -> colors) =
    (circle 0.005 & fillColor black)
    <> (circle 0.005 & fillColor black & translateX (fst (chromaStats colors)))
    <> mconcat (plotChroma1 <$> colors)
  where
    plotChroma1 c =
        circle 0.01
        & translateX (chroma c)
        & fillColor c

chroma :: Colour Double -> Double
chroma c =
    sqrt (a' * a' + b' * b')
  where
    (_, a, b) = cieLABView d65 c
    a' = a / 100
    b' = b / 100


hue :: Colour Double -> Double
hue c =
    atan2 b a
  where
    (_, a, b) = cieLABView d65 c


chromaStats :: (Foldable f, Functor f) => f (Colour Double) -> (Double, Double)
chromaStats colors =
    (rms, std)
  where
    squared x = x * x
    chromas = chroma <$> colors
    n = fromIntegral (length colors)
    mean = sum chromas / n
    rms = sqrt (sum (squared <$> chromas) / n)
    std = sqrt (sum (squared . (\chr -> chr - mean) <$> chromas) / n)


rmsChroma :: (Foldable f, Functor f) => f (Colour Double) -> Double
rmsChroma colors =
    rms
  where
    squared x = x * x
    chromas = chroma <$> colors
    n = fromIntegral (length colors)
    rms = sqrt (sum (squared <$> chromas) / n)


applyChroma :: Functor f => Double -> f (Colour Double) -> f (Colour Double)
applyChroma actual colors =
    applyChroma1 <$> colors
  where
    applyChroma1 c =
        cieLAB d65 l (a / original * actual) (b / original * actual)
      where
        (l, a, b) = cieLABView d65 c
        original = chroma c


fromCh :: Double -> Double -> Double -> Colour Double
fromCh l chr h = cieLAB d65 l (chr * cos h) (chr * sin h)
