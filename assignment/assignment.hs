import Data.Char(intToDigit)

-- In the first part of this assignment you'll be implementing 
-- functions that mimic the behavior of standard Haskell
-- library functions. Each section will make use of `foldl`
-- (remembering that `foldl` can return a list if you pass it a list
-- as the initial value, and build that list in the reducing function)

-- Remember to uncomment the tests for each section.

-------
-- 1 --
-------
-- The build in function `reverse` takes a list and returns
-- a new list with the elements in reverse order.
--
-- Implement a version vcalled `myreverse` using `foldl`.

-- myreverse :: [a] -> [a]
switch x y = y:x
myreverse a = foldl (switch) [] a

-------
-- 2 --
-------
-- The `filter` function takes a predicate function and a list. It returns
-- a new list containing only the elements of the original for which the
-- predicate returns `True`. Implement `myfilter` using `foldl`. (Hint: you 
-- might need to use `++` rather than `:` to build the result)

-- myfilter :: (a -> Bool) -> [a] -> [a]
checker func list x = 
  if func x
    then list++[x]
  else
    list  
myfilter func x= foldl (checker func) [] x
--i'm not really sure why i only have to pass one parameter into checker, but everytime i passed more i got errors.
-------
-- 3 --
-------
-- The `and` and `or` functions take a list of booleans.
-- `and` returns true only iff all the values are true.
-- `or` returns true if any are true.
--
-- (Side note: when implementing your functions, did you get a wavy
-- line suggestion say that you could replace your code with the
-- library function. Stop for a minute and see if you can work
-- out how Haskell worked this out.)
  
-- myand:: [ Bool ] -> Bool 

-- myor:: [ Bool ] -> Bool 

--andOr func first second = 
--  if first func second
--    then True
--    else False

--myand list = foldl (allTrue (&&)) True list
--myor list = foldl (andOr (||)) False list

--not really sure what went wrong here so I just have to do it a longer way

allTrue first second = 
  if first && second
    then True
    else False

myand list = foldl (allTrue) True list

anyTrue first second = 
  if first || second
    then True
    else False

myor list = foldl (anyTrue) False list


----------------------------------------------
--
-- Now we're going to play with some simple data types.


-------
-- 4 --
-------
-- Our application handles measurements in both millimeters (MM) and inches (IN)
-- write a data type named Measure that lets you specify a meansurement using
-- either unit. Be sure to derive Eq and Show.

data Measure = MM Float | IN Float

-- Now write a function that takes a measurement. If it is already in millimeters,
-- return it. Otherwise convert the measurement n inches to millimeters and
-- return a new metric Measure value. (Use 25.4 as the conversion factor)

ensureMetric (MM num) = num
ensureMetric (IN num) = num*25.4



-- Finally use `map` to take a list of Measure values, and return a new
-- list where all measurements are in metric.

--listToMetric:: [ Measure ] -> [ Measure ]

listToMetric list = map (ensureMetric) list


-----------------------------------
-- don't change below this point --

test :: (Eq a, Show a) => [Char] -> a -> a -> IO ()
test testNo got expected
  | got == expected =
      print (testNo ++ " OK   got expected value " ++ show expected)
  | otherwise =
      print (testNo ++ " FAIL expected " ++ show expected ++ " but got " ++ show got)


main = do

  -- test "1a" (myreverse []::[Int]) []
  -- test "1b" (myreverse [1])       [1]
  -- test "1c" (myreverse [1,2])     [2,1]
  -- test "1d" (myreverse "cat")     "tac"

  -- test "2a" (myfilter (>3) [1..6]) [4,5,6]
  -- test "2b" (myfilter even [1..6]) [2,4,6]
  -- test "2c" (myfilter (<0) [1..6]) []
  
  -- test "3a" (myand [True, True])   True
  -- test "3b" (myand [True, False])  False
  -- test "3c" (myand [False, True])  False
  -- test "3d" (myand [False, False]) False
  -- test "3e" (myand [True])         True

  -- test "3m" (myor  [True, True])   True
  -- test "3n" (myor  [True, False])  True
  -- test "3o" (myor  [False, True])  True
  -- test "3p" (myor  [False, False]) False
  -- test "3q" (myor  [True])         True

  -- test "4a" (ensureMetric (MM 16)) (MM 16)
  -- test "4b" (ensureMetric (IN 0))  (MM 0)
  -- test "4c" (ensureMetric (IN 1))  (MM 25.4)
  -- test "4c" (ensureMetric (IN 8))  (MM (8*25.4))
  -- test "4d" (listToMetric [ MM 1, IN 2, MM 3, IN 4]) [ MM 1, MM 50.8, MM 3, MM 101.6]
  print "Done"

  