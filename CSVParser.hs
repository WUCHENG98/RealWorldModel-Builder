module CSVParser where

import Language.Haskell.TH
import Data.List
import Data.Char


 -- splits the data read from the file
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t


 -- check if the string is a valid double
myisNumber :: String -> Bool
myisNumber ""  = False
myisNumber "." = False
myisNumber xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False


 -- wrap the data into the fitting library form
wrap :: [b] -> ([b], ([b], b))
wrap [a,b,c] = ([a],([b],c))


 -- nrcheck if x is "\n" or "\r"
nrcheck :: Char -> Bool
nrcheck x = x=='\n' || x=='\r'


 -- convert a list of data to type Double
map2double :: [String] -> [Double]
map2double = map (s2double)


 -- convert string to double if is valid
s2double :: String -> Double
s2double x
    | myisNumber  x = read x :: Double
    | otherwise = error "Csv contains non-numerical data"


 -- check #columns and whether there is value missing
checkcol :: Foldable t => [t a] -> [t a]
checkcol [] = []
checkcol (x:t)
    | (length x == 3) = (x: (checkcol t))
    | otherwise = error "Csv #columns does not equal 3 or there are value(s) missing"


 -- check if #rows is larger than 10
checkrow :: [a] -> [a]
checkrow [] = []
checkrow (x:t)
    | (length (x:t)) >= 10 = (x:t)
    | otherwise = error "Csv #rows does not meet the minimum requirement of 10, please provide more data groups"

 -- read data from the csv file
 -- fitdata format: [([x1],([y1],e1)), ([x2],([y2],e2)...([xn],([yn],en)))
 -- plotdata format: [[x1...xn], [y1...yn], [e1...en]]
readcsv :: FilePath -> IO [([Double], ([Double], Double))]
readcsv filename =
  do
    file <- readFile filename
    let rawdata = [splitsep (==',') line| line <- splitsep nrcheck file]
    let fdata = checkrow(checkcol (map (filter (/="")) rawdata))
    -- let transfdata = transpose fdata
    -- let plotdata = (map map2double  transfdata)
    let fitdata = (map wrap (map map2double fdata))
    return fitdata


