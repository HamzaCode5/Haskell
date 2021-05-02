import Data.List
import Data.Ord
import Text.Printf
-- MATHFUN
-- Haskell CW
-- UP938212
-- Types (define City type here)
data City = City String Int Int [Int]
  deriving (Show, Read, Eq, Ord)
--
testData :: [City]
testData = [  
    City "Amsterdam"  52   5    [1158, 1149, 1140, 1132],
    City "Athens"     38  23    [3153, 3153, 3154, 3156],
    City "Berlin"     53  13    [3567, 3562, 3557, 3552],
    City "Brussels"   51   4    [2096, 2081, 2065, 2050],
    City "Bucharest"  44  26    [1794, 1803, 1812, 1821],
    City "London"     52   0    [9426, 9304, 9177, 9046],
    City "Madrid"     40   4    [6669, 6618, 6559, 6497],
    City "Paris"      49   2    [11079, 11017, 10958, 10901],
    City "Rome"       42  13    [4278, 4257, 4234, 4210],
    City "Sofia"      43  23    [1284, 1281, 1277, 1272],
    City "Vienna"     48  16    [1945, 1930, 1915, 1901],
    City "Warsaw"     52  21    [1790, 1783, 1776, 1768]]
--
--  Your functional code goes here

-- grabs city name from the City test data
cityName :: City -> String
cityName (City cityName _ _ _) = cityName

-- grabs city latitude from the City test data
latitude :: City -> Int
latitude (City _ latitude _ _) = latitude

-- grabs city longitude from the City test data
longitude :: City -> Int
longitude (City _ _ longitude _) = longitude

-- grabs city populations from the City test data
population :: City -> [Int]
population (City _ _ _ population) = population

-- for exercise i
dataAt :: Int -> [a] -> a
dataAt _ [] = error "Empty List!"
dataAt y (x:xs)  | y <= 1 = x
                 | otherwise = dataAt (y-1) xs

-- for exercise ii
popToStringRtn :: [Int] -> String
popToStringRtn [] = "No Data"
popToStringRtn [x] = show (fromIntegral x / 1000 ) ++ "m"

-- for exercise iii
displayCity :: City -> String
displayCity city =
  cityName city
    ++ "    \t"
   ++ show (latitude city)
   ++ "  \t"
   ++ show (longitude city)
   ++ "  \t"
   ++ show (fromIntegral (population(city) !! 0) / 1000) ++ "m" ++ "   \t" ++
   show (fromIntegral (population(city) !! 1) / 1000) ++ "m" 

-- new populations data for exercise iv
newPopulations :: [Int]
newPopulations = [
    1200,
    3200,
    3600,
    2100,
    1800,
    9500,
    6700,
    11100,
    4300,
    1300,
    2000,
    1800]

-- for exercise vi
rtnGrowth :: [Double] -> String
rtnGrowth [] = ""
rtnGrowth (x:xs) = show (x * 100) ++ "%" ++ "\n" ++ rtnGrowth xs
-- Wasn't sure if Mathew Poole wanted it rounded but if so, replace "Show"--
  -- with printf "%.3f" to round to 3 decimal places --

-- ##################### END OF HELPER FUNCTIONS #####################

-- ########################### EXERCISES #############################
--exercise 1 (i)
citiesDisplayName :: [City] -> String
citiesDisplayName [] = ""
citiesDisplayName (x:xs) = cityName x ++ "\n" ++ citiesDisplayName xs

-- exercise 2 (ii)
populationRtn :: [City] -> String -> Int -> [Int]
populationRtn city name index = [ population (cityElement) !! index 
  | cityElement <- city, name == cityName (cityElement) ]

-- exercise 3 (iii)
citiesToString :: [City] -> String
citiesToString [] = ""
citiesToString [x] = displayCity x
citiesToString (x:xs) = displayCity x ++ "\n" ++ citiesToString xs

-- exercise 4 (iv)
updatePopulations :: [City] -> [City]
updatePopulations = go newPopulations

go :: [Int] -> [City] -> [City]
go [] cityString = cityString
go _ [] = []
go (i:is) ((City cityName long lat []):cityString) = 
  (City cityName long lat [i]) : go is cityString
go (i:is) ((City cityName long lat (p:ps)):cityString) = 
  (City cityName long lat (i:p:ps)) : go is cityString

-- exercise 5 (v)
cityNew :: String -> Int -> Int -> [Int] -> [City] -> [City]
cityNew cityName cityLat cityLon cityPopulations cityNew = 
 (City cityName cityLat cityLon cityPopulations) : cityNew
 
-- exercise 6 (vi)
percentageAnuCalc :: Int -> Int -> Double
percentageAnuCalc x y = (fromIntegral x - fromIntegral y) / fromIntegral y

annualPerGrowth :: [City] -> String -> Int -> [Double]
annualPerGrowth city name index = [
  (percentageAnuCalc (population (cityElement) !! (index - 1)) 
  (population (cityElement) !! (index))) 
  | cityElement <- city, 
  name == cityName (cityElement)
  ]

percentageAnuGrowth :: String -> [Double]
percentageAnuGrowth name = (annualPerGrowth testData name 1 ++ 
  annualPerGrowth testData name 2 ++ 
  annualPerGrowth testData name 3)

-- exercise 7 (vii)
closestBigCity :: [City] -> (Int, Int) -> Int -> String
closestBigCity cities location minPop = case sortedCities of
    [] -> "no city"
    (((City name _ _ _), _):_) -> name

  where

    sortedCities :: [(City, Double)]
    sortedCities = sortOn snd bigCitiesWithDistance

    bigCitiesWithDistance :: [(City, Double)]
    bigCitiesWithDistance = filter predicate citiesWithDistance

    predicate :: (City, Double) -> Bool
    predicate (City _ _ _ pops, _) =
      case pops of
        [] -> False
        (pop:_) -> pop > minPop

    citiesWithDistance :: [(City, Double)]
    citiesWithDistance = map (\city@(City _ lat long _) -> 
      (city, distanceCalc location (lat, long))) cities

distanceCalc :: (Int, Int) -> (Int, Int) -> Double
distanceCalc (ax, ay) (bx, by) = 
  sqrt (fromIntegral((ax - bx) ^ 2 + (ay - by) ^ 2))

-- ########################### DEMOS #############################
demo :: Int -> IO ()
demo 1 = putStrLn (citiesDisplayName testData)
demo 2 = putStrLn (popToStringRtn (populationRtn testData "Berlin" 0))
demo 3 = putStrLn (citiesToString testData)
demo 4 = putStrLn (citiesToString (updatePopulations testData))
demo 5 = putStrLn (citiesToString (sort(cityNew "Prague" 50 14 [1312, 1306, 1299, 1292] testData)))
demo 6 = putStrLn (rtnGrowth (percentageAnuGrowth "London")) 
demo 7 = putStrLn (closestBigCity testData (56,4) 2000)
type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
--
-- Your population map code goes here
--
--
-- Your user interface (and loading/saving) code goes here
--

loadCities :: IO [City]
loadCities = do
  contents <- readFile "cities.txt"
  return (read contents :: [City])

cityMenu :: [City] -> IO ()
cityMenu testData = do
  putStrLn ""
  putStrLn "Enter the number of the option desired: "
  putStrLn "1. Return All City Names"
  putStrLn "2. Return Population of City x Number of Years Ago"
  putStrLn "3. Return All Data In Neatly Formatted Columns"
  putStrLn "4. Update Data With New Population Figures"
  putStrLn "5. Add A New City To The List"
  putStrLn "6. Return A List of Annual Percentage Growth for x City "
  putStrLn "7. Return Closest City With A Population Bigger Than x number"
  putStrLn "8. Increase the sales of one album given its title and artist."
  putStrLn "0. Exit"
  option <- getLine
  executeOption option testData

executeOption :: String -> [City] -> IO ()
executeOption "1" testData = do
  putStrLn (citiesDisplayName testData)
  cityMenu testData
  
executeOption "2" testData = do
  putStrLn "Name of City?"
  fromStr <- getLine
  let cityName = read fromStr :: String
  putStrLn "From What Year"
  toStr <- getLine
  let popYear = read toStr :: Int
  let filteredCity = populationRtn testData cityName popYear 
  putStrLn (popToStringRtn filteredCity)
  cityMenu testData

executeOption "3" albumsList = do
  putStrLn (citiesToString testData)
  cityMenu testData

executeOption "4" albumsList = do
  putStrLn (citiesToString (updatePopulations testData))
  cityMenu testData

executeOption "5" testData = do
  putStrLn "Name of city you want added:"
  fromStr <- getLine
  let nameNew = read fromStr :: String
  putStrLn "Enter the longitude for the new city:"
  fromStr <- getLine
  let longNew = read fromStr :: Int
  putStrLn "Enter the latitude for the new city:"
  fromStr <- getLine
  let latNew = read fromStr :: Int
  putStrLn "Enter population numbers for the new city:"
  fromStr <- getLine
  let popNew = read fromStr :: [Int]
  let filteredCity = cityNew nameNew longNew latNew popNew testData
  putStrLn (citiesToString (sort filteredCity))
  cityMenu testData

executeOption "6" testData = do
  putStrLn "Name of City?"
  fromStr <- getLine
  let newcityName = read fromStr :: String
  let filteredCity =percentageAnuGrowth newcityName
  putStrLn (rtnGrowth filteredCity)
  cityMenu testData

executeOption "7" testData = do
  putStrLn "Enter the location of a city"
  fromStr <- getLine
  let location = read fromStr :: (Int, Int)
  putStrLn "Enter a population number"
  fromStr <- getLine
  let minPop = read fromStr :: Int
  let filteredCity = closestBigCity testData location minPop
  putStrLn (filteredCity)
  cityMenu testData

executeOption "0" testData = do
  writeFile "cities.txt" (show testData)

executeOption _ testData = do
  putStrLn "Select a valid option."
  cityMenu testData
 
-- UI
main :: IO ()
main = do
  testData <- loadCities
  putStrLn "Cities\n"
  putStrLn (citiesToString testData)
  cityMenu testData
