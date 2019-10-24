module CSVParser where

import Language.Haskell.TH
import Data.List
import Data.Char


 -- splits the data read from the file, from assignemt 3
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
    ('-':ys) -> all isDigit ys
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
s2double x = read x :: Double


-- check #columns and whether there is value missing
checkcol :: [[String]] -> Bool
checkcol [] = True 
checkcol (x:t)
    | (length x /= 3) = False
    | otherwise = checkcol t 
    
-- check if #rows is larger than 10
checkrow :: [[String]] -> Bool
checkrow lst = length lst >= 10


-- check if all is number, the input could never be empty
checknumber :: [[String]] -> Bool
checknumber [] = True
checknumber (h:s) = (foldr (\x y -> (myisNumber x) && y) True h) && (checknumber s)


-- read data from the csv file
-- fitdata format: [([x1],([y1],e1)), ([x2],([y2],e2)...([xn],([yn],en)))
-- plotdata format: [[x1...xn], [y1...yn], [e1...en]]
readcsv :: FilePath -> IO [([Double], ([Double], Double))]
readcsv filename =
  do
    file <- readFile filename
    let rawdata = [splitsep (==',') line| line <- splitsep nrcheck file]
    let fdata = (map (filter (/="")) rawdata)
    if ((checkcol fdata) && (checkrow fdata))
      then do 
        if (checknumber fdata)
          then do
            let fitdata = map wrap (map map2double fdata)
            return fitdata
          else do
            putStrLn "Your file contains wrong type (non-number)."
            putStrLn "Please modify your data file and enter the name again."
            filename <- getLineFixed
            readcsv filename              
      else do
        putStrLn "The number of columns or rows is not correct."
        putStrLn "Please modify your data file and enter the name again."
        filename <- getLineFixed
        readcsv filename

-- fix get line (delete and no empty), adopted from assignment 3, copy from ModelBuilder
getLineFixed :: IO [Char]
getLineFixed =
    do
      line <- getLine
      let res = (fixdel line)
      if (filter (/=' ') res) /= ""
        then return res
        else do
            putStrLn "This line has no content, please input again."
            newres <- getLineFixed
            return newres 

-- to determine if deletion is needed, copy from ModelBuilder
fixdel :: [Char] -> [Char]      
fixdel st
     | '\DEL' `elem` st = fixdel (remdel st)
     | otherwise = st

-- feature the delete fucntion, copy from ModelBuilder
remdel :: [Char] -> [Char]
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r

