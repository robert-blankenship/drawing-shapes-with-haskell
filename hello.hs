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
    let circlesX = 1
    let circlesY = 1

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

    let drawPolygon = \points -> do
            moveTo(points !! 0)
            foldr addLine (moveTo(points !! 0)) points
                where addLine (x, y) lines = do
                        lineTo(x, y)
                        lines
    
    let drawTriangle = \(x1, y1) (x2, y2) (x3, y3) -> do
        drawPolygon [(x1, y1), (x2, y2), (x3, y3)]
        stroke()

    -- Starting with some outer square defined by x, y, r, 
    -- draw an inscribed triangle pointing at angle (where 0 degrees
    -- is 'up')
    let reducedAngle angle = if angle > (2 * pi) then reducedAngle (angle - 2*pi) else angle

    let polygonAngles n = [(i * 2*pi/n) | i <- [0..n]]

    let regularPolygonAligned n angle = \x y s1 s2 -> do
            let s = (if s1 < s2 then s1 else s2) 
            let h' = minimum possibleHeights
                    where
                        getHeight angle'
                            | reducedAngle angle' < (pi/8) = abs (s / cos(angle'))
                            | reducedAngle angle' < (3*pi/4) = abs (s / sin(angle'))
                            | reducedAngle angle' < (5*pi/4) = abs (s / cos(angle'))
                            | reducedAngle angle' < (7*pi/4) = abs (s / sin(angle'))
                            | reducedAngle angle' <= (2*pi) = abs (s / cos(angle'))
    
                        angles = map (\angle' -> angle + angle') (polygonAngles n)
                        possibleHeights = map getHeight angles
    
            let point angle' = (x + (h' * cos(angle + angle')), y + (h' * sin(angle + angle')))
            let points = map (\a -> point a) (polygonAngles n)
    
            drawPolygon points
            stroke()
            strokeRect(x - s, y - s, 2*s, 2*s)

    -- TODO: This shouldn't draw the first angle twice.
    let regularPolygonAlignedMultiple n angle iterations = \x y s1 s2 -> do
            let initialPolygon = (regularPolygonAligned n angle x y s1 s2)
            let accumulatePolygons angle' accumulator = do
                    regularPolygonAligned n angle' x y s1 s2
                    accumulator

            foldr accumulatePolygons (initialPolygon) (polygonAngles iterations)
 
    let equilateralTriangleAligned angle = regularPolygonAligned 3 angle
    
    let trianglePointingDown = \x y rX rY ->
            drawTriangle (x - rX, y - rY) (x, y + rY) (x + rX, y - rY)
    
    let trianglePointingUp = \x y rX rY -> do
            drawTriangle (x - rX, y + rY) (x, y - rY) (x + rX, y + rY)
    
    let equilateralTrianglePointingUp = \x y rX rY ->
            if rX > rY
                then trianglePointingUp x y rY rY
                else trianglePointingUp x y rX rX
    
    let equilateralTrianglePointingDown = \x y rX rY ->
            if rX > rY
                then trianglePointingDown x y rY rY
                else trianglePointingDown x y rX rX

    let elem i j =
            (coordinate i (sceneWidth / (2 * fromInteger circlesX)),
             coordinate j (sceneHeight / (2 * fromInteger circlesY)),
             radius i j sceneWidth circlesX,
             radius i j sceneHeight circlesY,
             shape i j)
            where
                coordinate i' radius = radius + radius * 2 * fromInteger i'
                radius i j dimension circlesPerDimension = dimension / (2 * (fromInteger circlesPerDimension))
                shape i j
                    | otherwise = regularPolygonAlignedMultiple 4 pi 200

    let grid = [ elem i j | i <- [0..(circlesX - 1)], j <- [0..(circlesY - 1)] ]

    let drawElem (x, y, a, b, shapeMaker) accumulator = do
        beginPath()
        shapeMaker x y a b
        closePath()
        accumulator

    foldr drawElem (do beginPath()) grid 


