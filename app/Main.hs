{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (IOException, catch)

-- Calculate the factorial of a number
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- | Checks whether a number is prime
-- The function uses trial division to check for primality up to the square root
isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise = null [x | x <- [2 .. (n `div` 2)], n `mod` x == 0]

-- | Finds the nth prime number
-- The function generates an infinite list of prime numbers using the isPrime function,
-- and then takes the nth element of that list using the head and drop functions.
nthPrime :: Int -> Integer
nthPrime n = (head . drop (n - 1)) primes
  where
    primes = filter isPrime [2 ..]

main :: IO ()
main = do
  putStrLn "Enter a number to calculate its factorial:"
  num <- catch (readLn :: IO Integer) (\(_ :: IOException) -> return 0)
  putStrLn $ "The factorial of " ++ show num ++ " is: " ++ show (factorial num)
  putStrLn "Enter a number to find its prime rank:"
  num' <- catch (readLn :: IO Int) (\(_ :: IOException) -> return 0)
  let primeRank = nthPrime num'
  putStrLn $ "The " ++ show num' ++ "th prime number is: " ++ show primeRank
  putStrLn "Press Enter to exit..."
  getLine
  return ()
