module Main where

import Controller
import Model
import View
import Testing

-- | The list of all tests to run.
tests :: [Test]
tests = toolLabelTests ++ nextColourTests ++ nextToolTests

toolLabelTests :: [Test]
toolLabelTests =
  [ Test "LineTool"
      (assertEqual (toolToLabel (LineTool Nothing))
       "Line... click-drag-release")
  , Test "PolygonTool"
      (assertEqual (toolToLabel (PolygonTool []))
      "Polygon... click 3 or more times then spacebar")
  , Test "RectangleTool"
      (assertEqual (toolToLabel (RectangleTool Nothing))
      "Rectangle... click-drag-release")
  , Test "CircleTool"
      (assertEqual (toolToLabel (CircleTool Nothing))
      "Circle... click-drag-release")
  , Test "EllipseTool"
      (assertEqual (toolToLabel (EllipseTool Nothing))
      "Ellipse... click-drag-release")
  , Test "ParallelogramTool"
      (assertEqual (toolToLabel (ParallelogramTool Nothing Nothing))
      "Parallelogram... click two opposite vertices, then a third")
  ]

nextColourTests :: [Test]
nextColourTests =
  [ Test "Black -> Red" (assertEqual (nextColour Black) Red)
  , Test "Red -> Orange" (assertEqual (nextColour Red) Orange)
  , Test "Orange -> Yellow" (assertEqual (nextColour Orange) Yellow)
  , Test "Yellow -> Green" (assertEqual (nextColour Yellow) Green)
  , Test "Green -> Blue" (assertEqual (nextColour Green) Blue)
  , Test "Blue -> Purple" (assertEqual (nextColour Blue) Purple)
  , Test "Purple -> Black" (assertEqual (nextColour Purple) Black)
  ]

-- | Tests for nextTool, including tests that it doesn't cycle tools
-- midway through a draw.
nextToolTests :: [Test]
nextToolTests =
  [ Test "Line -> Polygon"
      (assertEqual (nextTool (LineTool Nothing)) (PolygonTool []))
  , Test "Polygon -> Rectangle"
      (assertEqual (nextTool (PolygonTool [])) (RectangleTool Nothing))
  , Test "Rectangle -> Circle"
      (assertEqual (nextTool (RectangleTool Nothing)) (CircleTool Nothing))
  , Test "Circle -> Ellipse"
      (assertEqual (nextTool (CircleTool Nothing)) (EllipseTool Nothing))
  , Test "Ellipse -> Parallelogram"
      (assertEqual (nextTool (EllipseTool Nothing)) (ParallelogramTool Nothing Nothing))
  , Test "Parallelogram -> Line"
        (assertEqual (nextTool (ParallelogramTool Nothing Nothing)) (LineTool Nothing))
  , Test "Line (in use) -> Line"
      (assertEqual (nextTool (LineTool (Just (1,1)))) (LineTool (Just (1,1))))
  , Test "Polygon (in use) -> Polygon"
      (assertEqual (nextTool (PolygonTool [(1,1)])) (PolygonTool [(1,1)]))
  , Test "Rectangle (in use) -> Rectangle"
      (assertEqual (nextTool (RectangleTool (Just (1,1)))) (RectangleTool (Just (1,1))))
  , Test "Circle (in use) -> Circle"
      (assertEqual (nextTool (CircleTool (Just (1,1)))) (CircleTool (Just (1,1))))
  , Test "Ellipse (in use) -> Ellipse"
      (assertEqual (nextTool (EllipseTool (Just (1,1)))) (EllipseTool (Just (1,1))))
  , Test "Parallelogram (in use, first point) -> Parallelogram"
      (assertEqual (nextTool (ParallelogramTool (Just (1,1)) Nothing)) 
      (ParallelogramTool (Just (1,1)) Nothing))
  , Test "Parallelogram (in use, second point) -> Parallelogram"
      (assertEqual (nextTool (ParallelogramTool Nothing (Just (1,1))))
      (ParallelogramTool Nothing (Just (1,1)))) 
  , Test "Parallelogram (in use, both points) -> Parallelogram"
      (assertEqual (nextTool (ParallelogramTool (Just (1,1)) (Just (2,2)))) 
      (ParallelogramTool (Just (1,1)) (Just (2,2))))
  ]

-- | A haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests
