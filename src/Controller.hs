--- Copyright 2020 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss t c) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> emptyModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show m)) m

      -- display the mystery image
      | k == "M" -> Model mystery t c

      | k == "Backspace" || k == "Delete" -> undefined  -- TODO: drop the last added shape

      | k == " " -> undefined  -- TODO: finish polygon vertices

      | k == "T" -> nextTool  -- TODO: switch tool

      | k == "C" -> nextColour  -- TODO: switch colour

      | k == "Left" -> undefined  -- TODO: rotate anticlockwise

      | k == "Right" -> undefined  -- TODO: rotate clockwise

      -- ignore other events
      | otherwise -> m
      where
        k = unpack key

    PointerPress p -> undefined  -- TODO

    PointerRelease p -> undefined  -- TODO
    _ -> m

-- TODO
nextColour :: ColourName -> ColourName
nextColour colour = case colour of
  Black  -> Red
  Red    -> Orange
  Orange -> Yellow
  Yellow -> Green
  Green  -> Blue
  Blue   -> Purple
  Purple -> Black

-- TODO
points = [Line, Polygon, Rectangle, Circle, Ellipse, Parallelogram]

nextTool :: Tool -> Tool
nextTool tool 
| map tool == points[i] points = points[i + 1]
  Nothing       -> points[0]
  []            -> points[0]
--  Line          -> Polygon 
--  Polygon       -> Rectangle
--  Rectangle     -> Circle
--  Circle        -> Ellipse
--  Ellipse       -> Parallelogram
--  Parallelogram -> Line