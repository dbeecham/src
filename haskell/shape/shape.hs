module Shape where

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving (Show)

type Radius = Float
type Side = Float
type Vertex = (Float, Float)

square s = Rectangle s s
circle r = Ellipse r r

regularPolygon :: Float -> Side -> Shape
regularPolygon n s = let pointFromAngle mul angle = ((sin angle) * mul, (cos angle) * mul)
                         tau = 2 * pi
                         angles n = [tau/n,tau*2/n..tau]
                     in Polygon $ map (pointFromAngle s) (angles n)

area :: Shape -> Float
area (Rectangle s1 s2) = s1 * s2
area (RtTriangle s1 s2) = s1 * s2 / 2
area (Polygon (v1:v2:v3:vs)) = triangleArea v1 v2 v3 + area (Polygon (v1:v3:vs))
    where triangleArea v1 v2 v3 = heron (len v1 v2) (len v2 v3) (len v3 v1)
          len (x1, y1) (x2, y2) = pythagoras (x2 - x1) (y2 - y1)
          pythagoras a b = sqrt $ (a ** 2) + (b ** 2)
          heron a b c = let s = (a + b + c)/2 in
            sqrt $ s * (s - a) * (s - b) * (s - c)
area (Polygon _) = 0
