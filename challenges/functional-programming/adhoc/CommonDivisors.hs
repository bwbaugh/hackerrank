{-
Common Divisors

Mario and Luigi earn points in their steps to save the Princess Peach
from a dragon. Let's denote Mario's points by M and Luigi's by L.
Princess Peach is wondering how many positive integers are there that
are divisors to both numbers, M and L. Help her find the answer.

Input
First line of input contains an integer, T, which represent the number
of test cases. Then follows T lines. Each line contains two space
separated integers, M L, representing the points earned by Mario and
Luigi, respectively.

Output
For each test case, print the solution in different lines.

Constraints
1 <= T <= 10
1 <= L, M <= 10^8
L, M are integers

Sample Input

3
10 4
1 100
288 240
Sample Output

2
1
10
Explanation
Test Case #00: Divisors of M = 10 are {1,2,5,10}, while for L = 4 they
are {1, 2, 4}. So M and L shares {1, 2} as their common divisors.

Test Case #01: Here as M = 1, both players only share this number as
their divisor.

Test Case #02: Here M and L shares 10 integers,
{1,2,3,4,6,8,12,16,24,48}, as their divisors.
-}
import Control.Applicative
import Control.Monad

main :: IO ()
main = readLn >>= flip replicateM_ testCase

testCase :: IO ()
testCase = fmap (map read . words) getLine >>= print . length . commonDivisors

commonDivisors :: [Int] -> [Int]
commonDivisors xs = filter (all (== 0) . (map mod xs <*>) . pure) minDivisors
  where
    minDivisors = go =<< candidates
    candidates = takeWhile (\x -> x <= (minXs `div` x)) [1..minXs]
    minXs = minimum xs
    go x = case minXs `divMod` x of
        (d, 0) -> if x == d then [x] else [x, d]
        _ -> []
