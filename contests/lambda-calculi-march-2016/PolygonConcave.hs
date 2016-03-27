{-
Concave Polygon

You are given the cartesian coordinates of a set of points in a 2D plane (in no particular order). Each of these points is a corner point of some Polygon, P, which is not self-intersecting in nature. Can you determine whether or not P is a concave polygon?

Input Format

The first line contains an integer, N, denoting the number of points.
The N subsequent lines each contain 2 space-separated integers denoting the respective x and y coordinates of a point.

Constraints

3≤N≤1000
0≤x,y≤1000
Output Format

Print YES if P is a concave polygon; otherwise, print NO.

Sample Input

4
0 0
0 1
1 1
1 0
Sample Output

NO
Explanation

The given polygon is a square, and each of its 4 internal angles are 90°. As none of these are over 180°, the polygon is not concave and we print NO.

Scoring

The percentage score awarded for your submission will be:

    100 - 2*(percentage of tests which you solve incorrectly)
If this value is negative, the percentage score for your submission will be 0.
So if you get half or more of the tests incorrect, your score will be a zero.
-}
import Control.Monad
import Data.Fixed (mod')
import Data.Function (on)
import Data.List (sortBy)
import Data.Tuple (swap)

type Point = (Double, Double)

main :: IO ()
main =
    readLn >>=
    flip replicateM readPoint >>=
    putStrLn . yesNo . isConcave . sortClockwise

readPoint :: IO Point
readPoint = fmap ((\[x, y] -> (x, y)) . map read . words) getLine

sortClockwise :: [Point] -> [Point]
sortClockwise = sortBy (compare `on` (uncurry atan2 . swap))

yesNo :: Bool -> String
yesNo True = "YES"
yesNo False = "NO"

isConcave :: [Point] -> Bool
isConcave = any (> 180) . interiorAngles

interiorAngles :: [Point] -> [Double]
interiorAngles points = map go [0..npoints - 1]
  where
    npoints = length points
    go i = (180 + theta1 - theta2 + 360) `mod'` 360
      where
        theta1 = atan2 y1 x1 * 180 / pi
        theta2 = atan2 y2 x2 * 180 / pi
        x1 = fst (points !! i) - fst (points !! previous)
        y1 = snd (points !! i) - snd (points !! previous)
        x2 = fst (points !! next) - fst (points !! i)
        y2 = snd (points !! next) - snd (points !! i)
        previous = (i - 1 + npoints) `mod` npoints
        next = (i + 1) `mod` npoints
