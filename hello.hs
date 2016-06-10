module Main where

import Graphics.Blank 
import Data.List
import Debug.Trace

main :: IO ()
main = blankCanvas 3333 { middleware=[] } $ go

go :: DeviceContext -> IO ()
go context = send context $ do
    globalAlpha 0.5
    drawPolkaDots (width context) (height context)

reducedAngle :: Double -> Double
reducedAngle angle = if angle > (2 * pi) then reducedAngle (angle - 2*pi) else angle

reducedAngles :: [Double] -> [Double]
reducedAngles angles = map reducedAngle angles

reorderedAngles :: [Double] -> [Double]
reorderedAngles angles = sort (reducedAngles angles)

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
            moveTo $ head points

            let addLine (x, y) lines = do
                    lineTo(x, y)
                    lines

            foldr addLine (lineTo $ head points) points
            stroke()

    let drawRegularPolygon = \(xCenter, yCenter) l n angle -> do
            let points = [ (x i, y i)  | i <- [0..(n-1)]]
                    where
                        angle' i = angle + (i * 2*pi/n)
                        x i = xCenter + l * sin(angle' i)
                        y i = yCenter + l * cos(angle' i)

            drawPolygon points
    
    let drawTriangle = \(x1, y1) (x2, y2) (x3, y3) -> do
            drawPolygon [(x1, y1), (x2, y2), (x3, y3)]

    -- Starting with some outer square defined by x, y, r, 
    -- draw an inscribed triangle pointing at angle (where 0 degrees
    -- is 'up')
    let polygonAngles n = [(i * 2*pi/n) | i <- [0..(n - 1)]]

    let getInscribedPolygonLength outerDiagonalLength outerAngles innerAngles = trace (show possibleHeights) $ minimum possibleHeights
            where
                getHeight angle' =
                     h_m / cos (abs (theta - theta_m))
                     where
                        majorAngle = if length (filter (angle' <=) outerAngles) > 0
                                    then head $ filter (angle' <=) outerAngles
                                    else head outerAngles + 2*pi
                        minorAngle = if length (filter (angle' >=) outerAngles) > 0
                                    then last $ filter (angle' >=) outerAngles
                                    else last outerAngles - 2*pi
                        theta = angle' - minorAngle
                        theta_m = (majorAngle - minorAngle) / 2
                        h_m = outerDiagonalLength * (cos theta_m)

                possibleHeights = map getHeight (reducedAngles innerAngles)

    let inscribedRegularPolygonAligned (outerSidesCount, outerAngleStart) (innerSidesCount, innerAngleStart) = \x y s1 s2 -> do
            let gridSideLength = (if s1 < s2 then s1 else s2)
            let gridAngles = reorderedAngles (map (+ pi/4) (polygonAngles 4))
            let gridDiagonalLength = gridSideLength * 1.414

            let outerAngles = reorderedAngles (map (+ outerAngleStart) (polygonAngles outerSidesCount))
            let outerDiagonalLength = getInscribedPolygonLength gridDiagonalLength gridAngles outerAngles

            let innerAngles = reorderedAngles (map (+ innerAngleStart) (polygonAngles innerSidesCount))
            let innerDiagonalLength = getInscribedPolygonLength outerDiagonalLength outerAngles innerAngles

            drawRegularPolygon (x,y) innerDiagonalLength innerSidesCount (head innerAngles)
            drawRegularPolygon (x,y) outerDiagonalLength outerSidesCount (head outerAngles)
    
    let inscribedRegularPolygonAlignedMultiple (outerSidesCount, outerAngleStart) innerSidesCount iterations = \x y s1 s2 -> do
            let alignedPolygon innerAngleStart' = inscribedRegularPolygonAligned (outerSidesCount, outerAngleStart) (innerSidesCount, innerAngleStart') x y s1 s2
            let accumulatePolygons innerAngleStart' accumulator = do
                    alignedPolygon innerAngleStart'
                    accumulator
            let initialPolygon = alignedPolygon outerAngleStart
            let otherPolygons = tail (map (+ outerAngleStart) (polygonAngles iterations))

            foldr accumulatePolygons initialPolygon otherPolygons
 
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
             coordinate j (sceneHeight / (2 * fromInteger circlesY)) ,
             radius i j sceneWidth circlesX,
             radius i j sceneHeight circlesY,
             shape i j)
            where
                coordinate i' radius = radius + radius * 2 * fromInteger i'
                radius i j dimension circlesPerDimension = dimension / (2 * (fromInteger circlesPerDimension))
                shape i j = inscribedRegularPolygonAlignedMultiple (6, pi) 6 19

    let grid = [ elem i j | i <- [0..(circlesX - 1)], j <- [0..(circlesY - 1)] ]

    let drawElem (x, y, a, b, shapeMaker) accumulator = do
        beginPath()
        shapeMaker x y a b
        closePath()
        accumulator

    foldr drawElem (do beginPath()) grid 


