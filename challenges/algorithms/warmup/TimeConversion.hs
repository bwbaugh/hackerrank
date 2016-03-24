{-
Given a time in AM/PM format, convert it to military (2424-hour) time.

Note: Midnight is 12:00:0012:00:00 AM on a 1212-hour clock and 00:00:0000:00:00 on a 2424-hour clock. Noon is 12:00:0012:00:00 PM on a 1212-hour clock and 12:00:0012:00:00 on a 2424-hour clock.

Input Format

A time in 12-hour clock format (i.e.: hh:mm:ssAM or hh:mm:ssPM), where 01≤hh≤1201≤hh≤12.

Sample Input

Convert and print the given time in 2424-hour format, where 00≤hh≤2300≤hh≤23.

Sample Output

07:05:45PM
Explanation

19:05:45
-}
import Data.List (intercalate)

main :: IO ()
main = getLine >>= putStrLn . to24Hour

to24Hour :: String -> String
to24Hour [h1, h2, ':', m1, m2, ':', s1, s2, meridian, 'M'] =
    intercalate ":" $ [hours meridian [h1, h2], [m1, m2], [s1, s2]]
  where
    hours 'P' "12" = "12"
    hours 'P' h = show . (+ 12) . read $ h
    hours 'A' "12" = "00"
    hours _ h = h
