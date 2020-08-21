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

      | k == "T" -> undefined  -- TODO: switch tool

      | k == "C" -> nextColour colour  -- TODO: switch colour

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
   otherwise -> Nothing

-- TODO
points = [LineTool Nothing, PolygonTool Nothing, RectangleTool Nothing, CircleTool Nothing, EllipseTool Nothing, ParallelogramTool Nothing]

nextTool :: Tool -> Tool
nextTool Maybe tool(Maybe point)(Maybe point)
-- base case 1
| point != [] = Just tool(Just point)
-- base case 2
| point != Nothing = Just tool(Just point)
-- recursive case
| tool (point) == Tool Nothing = case tool of
    LineTool Nothing                  -> PolygonTool []
    PolygonTool []                    -> RectangleTool
    RectangleTool Nothing             -> CircleTool Nothing
    CircleTool Nothing                -> EllipseTool Nothing
    EllipseTool Nothing               -> ParallelogramTool Nothing Nothing
    ParallelogramTool Nothing Nothing -> LineTool Nothing


