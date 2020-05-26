module Message where

rightMessage :: String
rightMessage = "Correct!"

wrongMessage :: String -> String
wrongMessage ans = "Wrong! Right answer: " ++ ans

baseMessage :: Int -> Int -> String
baseMessage number totalNumber = (show number) ++ "/" ++ (show totalNumber)