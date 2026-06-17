import System.Environment (getArgs)

-- Styling code below!!
red    = "\ESC[41m"
green = "\ESC[42m"
blue  = "\ESC[44m"
purple = "\ESC[45m"
cyan = "\ESC[46m"
white = "\ESC[47m"
yellow = "\ESC[43m"
reset  = "\ESC[0m"

style :: Int -> String
style 1 = red    ++ "  " ++ reset
style 2 = green  ++ "  " ++ reset
style 3 = blue   ++ "  " ++ reset
style 4 = purple ++ "  " ++ reset
style 5 = cyan   ++ "  " ++ reset
style 6 = yellow ++ "  " ++ reset
style 7 = white  ++ "  " ++ reset

-- Processing code below!!
fromString :: String -> [Int]
fromString [] = []
fromString (char:xs) =
    let fromChar '1' = 1
        fromChar '2' = 2
        fromChar '3' = 3
        fromChar '4' = 4
        fromChar '5' = 5
        fromChar '6' = 6
        fromChar '7' = 7
        fromChar _ = error ("Character " ++ [char] ++ " is not a valid state!")
    in (fromChar char) : fromString xs

data Layer = Layer [Maybe Int]

type Combine = Int -> Int -> Int

next :: Combine -> Layer -> Layer
next combine (Layer xs) = Layer $ map (combineFromRules combine) (zip (xs++[Nothing]) (Nothing:xs))

instance Show Layer where
    show (Layer []) = ""
    show (Layer (Nothing:xs)) = "  " ++ show (Layer xs)
    show (Layer ((Just s):xs)) = style s ++ show (Layer xs)

showLayers :: [Layer] -> IO ()
showLayers layers =
    let width = (let Layer lastLayer = last layers in length lastLayer) in
    let printLayer height [] = return ()
        printLayer height (l:xs) = do
                putStrLn $ concat (replicate (height - 1) " ") ++ show l
                printLayer (height-1) xs
    in printLayer width layers

extendN :: Combine -> Int -> Layer -> [Layer]
extendN combine 0 _ = error "Size can't be zero"
extendN combine 1 x = [x]
extendN combine n x = x : extendN combine (n-1) (next combine x)

combineFromRules :: (Int -> Int -> Int) -> (Maybe Int, Maybe Int) -> Maybe Int
combineFromRules runRule (Nothing, Just 1) = Just 1
combineFromRules runRule (Just 1, Nothing) = Just 1
combineFromRules runRule (Just a, Just b) = Just $ runRule a b

applyRule :: [Int] -> (Int -> Int -> Int)
applyRule ruleSet a b | a > b = applyRule ruleSet b a -- swap for canonical order
applyRule ruleSet a b =
    let pos = (a-1) + (b*(b-1) `div` 2) in -- this computes position according to canonica order
    if pos >= length ruleSet then error ("Error! No rule for " ++ show a ++ " + " ++ show b)
    else ruleSet !! pos

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "No ruleset provided!"
        (rules:rest) ->
            let ruleSet = fromString rules in
            let depth = case rest of
                    [] -> 60 -- default depth
                    (custom:_) -> read custom
            in showLayers $ extendN (applyRule ruleSet) depth (Layer [Just 1])

