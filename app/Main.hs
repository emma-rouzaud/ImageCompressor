import System.Exit
import System.Environment
import Data.List
import System.IO
import Text.Read

exitMsg :: ExitCode -> IO ()
exitMsg e = putStrLn "USAGE: ./imageCompressor n e IN \
                   \\n\n      n       number of colors in the final image \
                   \\n      e       convergence limit \
                   \\n      IN      path to the file containing the colors of the pixels" >> exitWith e

removeAllChars :: Eq a => [a] -> [a] -> [a]
removeAllChars = filter . flip notElem

getElemAtIndex :: [a] -> Int -> a
getElemAtIndex list index = head (drop index list)

removeAllBeforeNb :: String -> String
removeAllBeforeNb = tail . dropWhile (/=',')

getUnparsedArray :: Int -> String -> [String]
getUnparsedArray n str = take n $ iterate removeAllBeforeNb str

getValueArray :: Int -> String -> [String]
getValueArray n str = map (takeWhile (/= ',')) (getUnparsedArray n (removeAllChars "()" str))

getInt :: String -> Float
getInt str = read str :: Float

parseTupple :: Int -> String -> [Float]
parseTupple n str = map getInt (getValueArray n str)

data Color = Color Float Float Float
instance Show Color where
    show (Color r g b) = "(" ++ (takeWhile (/= '.') (show r)) ++ "," ++ (takeWhile (/= '.') (show g)) ++ "," ++ (takeWhile (/= '.') (show b)) ++ ")"

data Position = Position Float Float
instance Show Position where
    show (Position x y) = "(" ++ (takeWhile (/= '.') (show x)) ++ "," ++ (takeWhile (/= '.') (show y)) ++ ")"

data Pixel = Pixel Position Color
instance Show Pixel where
    show (Pixel pos color) = (show pos) ++ " " ++ (show color)  ++ "\n"

data Cluster = Cluster Color Color [Pixel]
instance Show Cluster where
    show (Cluster old new list) = "--\n" ++ showClusterColor new ++ "\n-\n"

showClusterColor :: Color -> [Char]
showClusterColor (Color r g b) = "(" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")"

getColor :: String -> Color
getColor str = Color (head tab) (head (tail tab)) (last tab)
    where tab = parseTupple 3 str

getPosition :: String -> Position
getPosition str = Position (head tab) (last tab)
    where tab = parseTupple 2 str

addPixel :: String -> Pixel
addPixel line = Pixel (getPosition (head (tab))) (getColor (last (tab)))
    where
        tab = words (removeAllChars "()" line)

createPixelTab :: [String] -> [Pixel]
createPixelTab tab = map addPixel tab

getDistance :: Color -> Color -> Float
getDistance (Color xa ya za) (Color xb yb zb) = sqrt ((xa - xb)^2 + (ya - yb)^2 + (za - zb)^2)

findValueInTab :: [Pixel] -> Int -> Int -> Pixel
findValueInTab tab k n = getElemAtIndex tab ((((length tab) * n) `div` k) - 1)

getPixelColor :: Pixel -> Color
getPixelColor (Pixel pos color) = color

getColorR :: Color -> Float
getColorR (Color r g b) = r

getColorG :: Color -> Float
getColorG (Color r g b) = g

getColorB :: Color -> Float
getColorB (Color r g b) = b

getNewAverageColor :: Cluster -> Color
getNewAverageColor (Cluster old new list) = new

getPixelList :: Cluster -> [Pixel]
getPixelList (Cluster old new list) = list

createCluster :: [Pixel] -> Int -> Int -> Cluster
createCluster tab k n = Cluster (Color 0 0 0) (getPixelColor (findValueInTab tab k n)) []

findRandomClusters :: [Pixel] -> Int -> [Cluster]
findRandomClusters tab k= map (createCluster tab k) [1..k]

checkSpaces :: String -> Bool
checkSpaces str = if length (words str) == 2 then True else False

checkCommas :: String -> Int -> Bool
checkCommas str n = if length (filter (== ',') str) == n then True else False

checkLine :: String -> Bool
checkLine file
    | checkSpaces file == False = False
    | checkCommas (head (words file)) 1 == False = False
    | checkCommas (last (words file)) 2 == False = False
    | otherwise = True

parseFile :: String -> Int -> Float-> IO ()
parseFile file k e = do
    content <- readFile file
    if and (map (checkLine) (lines content)) then startAlgorithm (createPixelTab (lines content)) k e else exitMsg (ExitFailure 84)

getAllDistances :: [Cluster] -> Pixel -> [Float]
getAllDistances clusterTab pixel = map (getDistance (getPixelColor pixel)) (map getNewAverageColor clusterTab)

findSmallest :: [Float] -> Float
findSmallest tab = foldr1 min tab

getIndexOf :: Float -> [Float] -> Int -> Int
getIndexOf goal list index
    | getElemAtIndex list index == goal = index
    | otherwise = getIndexOf goal list (index + 1)

addToPixelList :: Cluster -> Pixel -> Cluster
addToPixelList (Cluster old new list) pixel = Cluster old new (list ++ [pixel])

updateClusterList :: [Cluster] -> Pixel -> [Cluster]
updateClusterList clusterTab pixel = take n clusterTab ++ [cluster] ++ drop (n + 1) clusterTab
    where
        distTab = getAllDistances clusterTab pixel
        n = getIndexOf (findSmallest distTab) distTab 0
        cluster = addToPixelList (getElemAtIndex clusterTab n) pixel

fillPixelList :: [Pixel] -> [Cluster] -> [Cluster]
fillPixelList pixelTab [] = []
fillPixelList [] clusterTab = clusterTab
fillPixelList pixelTab clusterTab = fillPixelList (tail pixelTab) (updateClusterList clusterTab (head pixelTab))

startAlgorithm :: [Pixel] -> Int -> Float -> IO ()
startAlgorithm [] k e = exitMsg (ExitFailure 84)
startAlgorithm pixelTab k e = printClusters (loop e pixelTab (map updateClusterColors (fillPixelList pixelTab (findRandomClusters pixelTab k))))

calculateAverage :: [Float] -> Float
calculateAverage tab = if (foldl (+) 0 tab) == 0 then 0 else (foldl (+) 0 tab) / (fromIntegral (length tab))

getColorAverage :: [Color] -> Color
getColorAverage colorTab = Color (calculateAverage (map (getColorR) colorTab)) (calculateAverage (map (getColorG) colorTab)) (calculateAverage (map (getColorB) colorTab))

updateClusterColors :: Cluster -> Cluster
updateClusterColors (Cluster old new list) = (Cluster new (getColorAverage (map getPixelColor list)) list)

compareClustersColors :: Float -> Cluster -> Bool
compareClustersColors e (Cluster old new list)
    | getDistance old new > e = False
    | otherwise = True

filterClusters :: Float -> [Cluster] -> Bool -> [Cluster]
filterClusters e [] ok = []
filterClusters e clusterTab ok = filter (\cluster -> compareClustersColors e cluster == ok) clusterTab

checkIfOver :: Float -> [Cluster] -> Bool
checkIfOver e clusterTab = and (map (compareClustersColors e) clusterTab)

clearClusterList :: Cluster -> Cluster
clearClusterList (Cluster old new list) = (Cluster old new [])

loop :: Float -> [Pixel] -> [Cluster] -> [Cluster]
loop e pixelTab clusterTab
    | checkIfOver e clusterTab == False = loop e pixelTab (map updateClusterColors (fillPixelList pixelTab (map clearClusterList (clusterTab))))
    | otherwise = clusterTab

printClusterList :: Cluster -> [Char]
printClusterList (Cluster old new list) = concatMap show list

printClusters :: [Cluster] -> IO ()
printClusters clusterTab = putStrLn (concatMap (\cluster -> show cluster ++ (printClusterList cluster)) clusterTab)

start :: [String] -> IO ()
start [] = exitMsg (ExitFailure 84)
start [k, e, path] =
    case readMaybe k of
        Just x -> if x > 0
            then case readMaybe e of
                Just y -> if y >= 0 then parseFile path x y else exitMsg (ExitFailure 84)
                Nothing -> exitMsg (ExitFailure 84)
            else exitMsg (ExitFailure 84)
        Nothing -> exitMsg (ExitFailure 84)
start tab = exitMsg (ExitFailure 84)

main :: IO ()
main = start =<< getArgs
