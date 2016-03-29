{-
Pascal's Triangle

For a given integer KK, print the first KK rows of Pascal's Triangle.
Print each row with each value separated by a single space. The value
at the nnthth row and rrthth column of the triangle is equal to
n!/(r!∗(n−r)!)n!/(r!∗(n−r)!) where indexing starts from 00. These
values are the binomial coefficients.

The Pascal Triangle

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
....
Input Format

A single line of input, integer KK.

Constraints

2<=K<=102<=K<=10

Output Format

Output the first KK rows of Pascal's triangle.

Sample Input

4
Sample Output

1
1 1
1 2 1
1 3 3 1
-}
main :: IO ()
main = readLn >>= putStr . unlines . map (unwords . map show) . pascal

pascal :: Int -> [[Int]]
pascal n = map pascalRow [0..n-1]

pascalRow :: Int -> [Int]
pascalRow n = map (pascalColumn n) [0..n]

pascalColumn :: Int -> Int -> Int
pascalColumn n r =
    floor $ product [1..n'] / (product [1..r'] * product [1..n' - r'])
  where
    n' = fromIntegral n
    r' = fromIntegral r
