-- Parsing 
-- In general, a parser is a program that processes structured input (like a string) and extracts meaningful information from it. In the example you've provided,
-- the input is a string, and the parser either succeeds by consuming part of the string and returning some result or fails by returning Nothing.

-- Simple Parsers 
--You start by defining a parser for characters and a parser for integers, both of which return Maybe (rest of the string, result)
--parseChar: This function takes a string and tries to extract the first character. If the string is empty, it returns Nothing.
parseChar :: String -> Maybe (String, Char)
parseChar "" = Nothing -- if it is a empty String then it will return Nothing 
parseChar (c:rest) = Just (rest, c)
-- c:rest mean, it attract the first element of the string 
-- c: the first element 
-- rest: all the element except the first element 

-- ParseInt 
--parseInt: This function uses the reads function to parse an integer from the input string and returns the rest of the string and the parsed integer if successful.
parseInt :: String -> Maybe (String, Int)
parseInt s = case reads s of
    [(x, rest)] -> Just (rest, x)
    _           -> Nothing



