module Main where

import Data.Default.Class (def)
import Data.Function ((&))
import Control.Lens ((.~), (.=))
import Graphics.Rendering.Chart.Easy (
    Renderable(..), ToPlot(toPlot), EC(..), Layout(..)
  , toRenderable, toPlot, plot, plotLeft, plotRight, line
  , layout_title
  , layoutlr_title, layoutlr_x_axis, layoutlr_left_axis, layoutlr_right_axis
  , layoutlr_plots, layoutlr_grid_last
  , laxis_override, axisGridHide
  , plot_lines_title, plot_lines_values
  )
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile, toFile)

main :: IO ()
main = do
  let xs1 = [[(1, 1.0) , (2, 2.0), (3, 3.0), (4, 4.0)]] :: [[(Int, Double)]]
  let xs2 = [[(1, 1.0) , (2, 2.0), (3, 4.0), (4, 8.0)]] :: [[(Int, Double)]]
  toFile def "first.svg" $ do
    layoutlr_title .= "Price History"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotLeft (line "price 1" xs1)
    plotRight (line "price 2" xs2)
    
  toFile def "second.svg" $ do
    layout_title .= "Simulation of betting on a biased coin"
    plot (line "f=0.5" xs1)
  putStrLn "finished"