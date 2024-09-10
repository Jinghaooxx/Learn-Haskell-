
fib :: Int -> Int -- The return statement, Int (What it takes in) -> Int (What it return)
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

main :: IO ()
main = print $ map fib [1..10]
-- $ is used to avoid Parenthesis 
-- in this case instead of doing print(map fib [1..10]) we can replace it with $
-- The main basically act as the main function 
