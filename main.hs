depth = 150

ruleSet = fromString "231331"

-- Recursive:
    -- Sierpinski's triangle: 212
    -- hexagon (recursive): 213321 (golden trio)
    -- Stripped (1-2) sierpinski's triangle: 231331  (shown at the end of Part 1)
    -- Stripped (2-3) sierpinski's triangle: 213312
    -- Honeycomb triangle: 2134232431

-- Repetitive:
    -- shell rainbow: 223131 (golden trio)
    -- shrinking pillars: 2134212143

-- Chaotic:
    -- 231131  (shown at the end of Part 1)
    -- 231121
    -- 2432143241  (variant: 2432143421)
    -- 2134212431  (might be recursive but I don't think so)


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

applyRule :: Int -> Int -> Int
applyRule a b | a > b = applyRule b a -- swap for canonical order
applyRule a b =
    let pos = (a-1) + (b*(b-1) `div` 2) in -- this computes position according to canonica order
    if pos > length ruleSet then error ("Error! No rule for " ++ show a ++ " + " ++ show b)
    else ruleSet !! pos

data Layer = Layer [Maybe Int]
combine :: (Maybe Int, Maybe Int) -> Maybe Int
combine (Nothing, Just 1) = Just 1
combine (Just 1, Nothing) = Just 1
combine (Just a, Just b) = Just $ applyRule a b

next :: Layer -> Layer
next (Layer xs) = Layer $ map combine (zip (xs++[Nothing]) (Nothing:xs))

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

extendN :: Int -> Layer -> [Layer]
extendN 0 _ = error "Size can't be zero"
extendN 1 x = [x]
extendN n x = x : extendN (n-1) (next x)

main = showLayers $ extendN depth (Layer [Just 1])

