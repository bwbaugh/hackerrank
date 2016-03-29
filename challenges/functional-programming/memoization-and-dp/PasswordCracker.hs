{-
Password Cracker FP

There are N users registered on a website CuteKittens.com. Each of them
have a unique password represented by pass[1], pass[2], ..., pass[N].
As this a very lovely site, many people want to access those awesomely
cute pics of the kittens. But the adamant admin don't want this site to
be available for general public. So only those people with passwords
can access it.

Yu being an awesome hacker finds a loophole in their password
verification system. A string which is concatenation of one or more
passwords, in any order, is also accepted by the password verification
system. Any password can appear 0 or more times in that string. He has
access to each of the N passwords, and also have a string loginAttempt,
he has to tell whether this string be accepted by the password
verification system of the website.

For example, if there are 3 users with password {"abra", "ka",
"dabra"}, then some of the valid combinations are "abra" (pass[1]),
"kaabra" (pass[2]+pass[1]), "kadabraka" (pass[2]+pass[3]+pass[2]),
"kadabraabra" (pass[2]+pass[3]+pass[1]) and so on.

Input
First line contains an integer T, the total number of test cases. Then
T test cases follow.

First line of each test case contains N, the number of users with
passwords. Second line contains N space separated strings, pass[1]
pass[2] ... pass[N], representing the passwords of each user. Third
line contains a string, loginAttempt, for which Yu has to tell whether
it will be accepted or not.

Output
For each valid string, Yu has to print the actual order of passwords,
separated by space, whose concatenation results into loginAttempt. If
there are multple solutions, print any of them. If loginAttempt can't
be accepted by the password verification system, then print WRONG
PASSWORD.

Constraints
1 <= T <= 10
1 <= N <= 10
pass[i] ≠ pass[j], 1 <= i < j <= N
1 <= length(pass[i]) <= 10, where i ∈ [1, N]
1 < length(loginAttempt) <= 2000

loginAttempt and pass[i] contains only lowercase latin characters
('a'-'z').

Sample Input

3
6
because can do must we what
wedowhatwemustbecausewecan
2
hello planet
helloworld
3
ab abcd cd
abcd
Sample Output

we do what we must because we can
WRONG PASSWORD
ab cd
Explanation 
Sample Case #00: "wedowhatwemustbecausewecan" is the concatenation of
passwords {"we", "do", "what", "we", "must", "because", "we", "can"}.
That is

loginAttempt = pass[5] + pass[3] + pass[6] + pass[5] +  pass[4] +
pass[1] + pass[5] + pass[2]
Note that any password can repeat any number of times.

Sample Case #01: We can't create string "helloworld" using the strings
{"hello", "planet"}.

Sample Case #02: There are two ways to create loginAttempt ("abcd").
Both pass[2] = "abcd" and pass[1] + pass[3] = "ab cd" are valid
answers.
-}
import Control.Monad
import Data.List (isPrefixOf)

main :: IO ()
main = readLn >>= flip replicateM_ testCase

testCase :: IO ()
testCase = do
    _ <- getLine
    passwords <- fmap words getLine
    guess <- getLine
    case sequence (loginAttempt passwords guess) of
        Nothing -> putStrLn "WRONG PASSWORD"
        Just xs -> putStrLn $ unwords xs

loginAttempt :: [String] -> String -> [Maybe String]
loginAttempt _ [] = []
loginAttempt passwords guess = case matching of
    [] -> [Nothing]
    (x:_) -> Just x : loginAttempt passwords (drop (length x) guess)
  where
    matching = filter (flip isPrefixOf guess) passwords
