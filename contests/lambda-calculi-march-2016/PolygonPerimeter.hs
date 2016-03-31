{-
Compute the Perimeter of a Polygon

You are given the cartesian coordinates of a set of points in a 2D plane. When traversed sequentially, these points form a Polygon, P, which is not self-intersecting in nature. Can you compute the perimeter of polygon P?

Input Format

The first line contains an integer, N, denoting the number of points.
The N subsequent lines each contain 2 space-separated integers denoting the respective x and y coordinates of a point.

Constraints

No 2 points are coincident, and polygon P is obtained by traversing the points in a clockwise direction.
3≤N≤1000
0≤x,y≤1000
Output Format

For each test case, print the perimeter of P (correct to a scale of one decimal place).

Note: Do not add any leading/trailing spaces or units.

Sample Input

4
0 0
0 1
1 1
1 0
Sample Output

4
Explanation

The given polygon is a square, and each of its sides are 1 unit in length. perimeter(P)=1+1+1+1=4, so we print 4 on a new line.
-}
import Control.Monad

type Point = (Double, Double)

main :: IO ()
main = readLn >>= flip replicateM readPoint >>= print . perimeter

readPoint :: IO Point
readPoint = fmap ((\[x, y] -> (x, y)) . map read . words) getLine

perimeter :: [Point] -> Double
perimeter xs = sum $ zipWith distance xs (tail xs ++ [head xs])

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2
