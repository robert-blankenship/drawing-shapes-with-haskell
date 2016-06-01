module Main where

import Graphics.Blank 
import Debug.Trace

main :: IO ()
main = blankCanvas 3333 { middleware=[] } $ go

go :: DeviceContext -> IO ()
go context = send context $ do
    globalAlpha 0.5
    drawPolkaDots (width context) (height context)

drawPolkaDots :: Double -> Double -> Canvas ()
drawPolkaDots sceneWidth sceneHeight = do
    let circlesPerDimension = 10 :: Integer

    let ellipse = \x y rX rY -> do
                let e = 4 / 3 * tan(pi/4)
                moveTo(x, y + rY)
                bezierCurveTo((x + e*rX), (y + rY), (x + e*rX), (y - rY), x, y - rY)
                bezierCurveTo((x - e*rX), (y - rY), (x - e*rX), (y + rY), x, y + rY)
                fill()

    let circle = \x y rX rY -> do
                moveTo(x, y)
                arc(x, y, (if rX < rY then rX else rY), 0, 2*pi, False)
                fill()
    
    let rectangle = \x y rX rY ->
            fillRect(x - rX, y - rY, 2 * rX, 2 * rY)

    let square = \x y rX rY -> do
                if rX > rY
                    then fillRect(x - rY, y - rY, 2 * rY, 2 * rY)
                    else fillRect(x - rX, y - rX, 2 * rX, 2 * rX)

    let triangle = \x y rX rY -> do
                if rX > rY
                    then fillRect(x - rY, y - rY, 2 * rY, 2 * rY)
                    else fillRect(x - rX, y - rX, 2 * rX, 2 * rX)


    let elem i j =
            (coordinate i (sceneWidth / (2 * fromInteger circlesPerDimension)),
             coordinate j (sceneHeight / (2 * fromInteger circlesPerDimension)),
             radius i j sceneWidth,
             radius i j sceneHeight,
             shape i j)
            where
                coordinate i' radius = radius + radius * 2 * fromInteger i'
                radius i j dimension = dimension / (2 * (fromInteger circlesPerDimension))
                shape i j
                    | i `mod` 2 == 0 = ellipse
                    | otherwise = square

    let grid = [ elem i j | i <- [0..(circlesPerDimension - 1)], j <- [0..(circlesPerDimension - 1)] ]

    let drawElem (x, y, a, b, shapeMaker) accumulator = do
        beginPath()
        shapeMaker x y a b
        closePath()
        accumulator

    foldr drawElem (do beginPath()) grid 


