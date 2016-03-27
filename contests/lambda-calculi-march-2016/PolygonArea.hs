{-
Compute the Area of a Polygon

You are given the cartesian coordinates of a set of points in a 2D plane. When traversed sequentially, these points form a Polygon, P, which is not self-intersecting in nature. Can you compute the area of polygon P?

Input Format

The first line contains an integer, N, denoting the number of points.
The N subsequent lines each contain 2 space-separated integers denoting the respective x and y coordinates of a point.

Constraints

No 2 points are coincident, and polygon P is obtained by traversing the points in a clockwise direction.
4≤N≤1000
0≤x,y≤1000
Output Format

For each test case, print the area of P (correct to a scale of one decimal place).

Note: Do not add any leading/trailing spaces or units; it is assumed that your result is in square units.

Sample Input

4
0 0
0 1
1 1
1 0
Sample Output

1
Explanation

The given polygon is a square, and each of its sides are 1 unit in length.
area(P)=length×width=1×1=1, so we print 1 on a new line.
-}
import Control.Monad

type Point = (Double, Double)

main :: IO ()
main = readLn >>= flip replicateM readPoint >>= print . area

readPoint :: IO Point
readPoint = fmap ((\[x, y] -> (x, y)) . map read . words) getLine

area :: [Point] -> Double
area points = (xys - yxs) / 2
  where
    xys = sum $ zipWith (*) xs (tail ys ++ [head ys])
    yxs = sum $ zipWith (*) ys (tail xs ++ [head xs])
    xs = map fst points
    ys = map snd points
