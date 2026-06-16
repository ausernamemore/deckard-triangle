applyRule :: Int -> Int -> Int
applyRule 1 1 = 2

applyRule 1 2 = 3
applyRule 2 2 = 1

applyRule 1 3 = 1
applyRule 2 3 = 2
applyRule 3 3 = 1

applyRule 1 4 = 2
applyRule 2 4 = 3
applyRule 3 4 = 4
applyRule 4 4 = 1

-- Sierpinski's triangle: 212
-- Golden trio patterns:
    -- rainbow (repetitive): 223131
    -- hexagon (recursive): 213321

-- Stripped (1-2) sierpinski's triangle: 231331  (shown at the end of Part 1)
    -- Stripped (2-3) sierpinski's triangle: 213312
-- Chaotic: 231131  (shown at the end of Part 1)
    -- Trippy variant: 231121

applyRule a b = applyRule b a -- make all rules commutative



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

-- Main stuff below!!!
depth = 80

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

