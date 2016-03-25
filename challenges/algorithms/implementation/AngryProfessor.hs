{-
A Discrete Mathematics professor has a class of N students. Frustrated with their lack of discipline, he decides to cancel class if fewer than K students are present when class starts.

Given the arrival time of each student, determine if the class is canceled.

Input Format

The first line of input contains T, the number of test cases.

Each test case consists of two lines. The first line has two space-separated integers, N (students in the class) and K (the cancelation threshold). 
The second line contains N space-separated integers (a1,a2,…,aN) describing the arrival times for each student.

Note: Non-positive arrival times (ai≤0) indicate the student arrived early or on time; positive arrival times (ai>0) indicate the student arrived ai minutes late.

Output Format

For each test case, print the word YES if the class is canceled or NO if it is not.

Constraints

1≤T≤10
1≤N≤1000
1≤K≤N
−100≤ai≤100,where i∈[1,N]
Note 
If a student arrives exactly on time (ai=0), the student is considered to have entered before the class started.

Sample Input

2
4 3
-1 -3 4 2
4 2
0 -1 2 1
Sample Output

YES
NO
Explanation

For the first test case, K=3. The professor wants at least 3 students in attendance, but only 2 have arrived on time (−3 and −1). Thus, the class is canceled.

For the second test case, K=2. The professor wants at least 2 students in attendance, and there are 2 who have arrived on time (0 and −1). Thus, the class is not canceled.
-}
import Control.Monad

main :: IO ()
main = readLn >>= flip replicateM_ testCase

testCase :: IO ()
testCase = do
    [_, threshold] <- fmap (map read . words) getLine
    arrivals <- fmap (map read . words) getLine
    if isCanceled threshold arrivals
        then putStrLn "YES"
        else putStrLn "NO"

isCanceled :: Int -> [Int] -> Bool
isCanceled threshold = null . drop (threshold - 1) . filter (<= 0)
