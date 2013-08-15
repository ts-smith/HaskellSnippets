module Vectors where

type Vector = (Double, Double)
type Angle = Double
type XValue = Double

rad :: Angle -> Angle
rad degree = degree / 180 * pi

deg :: Angle -> Angle
deg radian = radian / pi * 180

dot :: Vector -> Vector -> XValue
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

normalize :: Vector -> Vector
normalize (x,y) = (x/magnitude, y/magnitude)
    where magnitude = sqrt (x * x + y * y)


v1 = normalize (2,2)

v2 = normalize (3,2)

xvec = normalize (1,0)

v3 = normalize (-2,2)
v4 = normalize (2,2)

--vectors = [v1,v2,xvec,v3,v4]

v5 = normalize (2,2)
v6 = normalize (0,1)
v7 = normalize (1,0)

vectors = [v5,v6, v7]

(ff,_) = normalize (1,1)

completeList = [
         [(0,1), (0.5,0.5)]
        ,[normalize (1,1), (0,ff)]
        --,[(1,0), (0.5,0.5)]
        --,[normalize (1,1), (ff,0)]
    ]



--cos :: Angle -> XValue
--acos :: XValue -> Angle

angle :: Vector -> Vector -> Angle
angle v1 v2 = deg $ acos x
    where n1 = normalize v1
          n2 = normalize v2
          x = dot n1 n2
