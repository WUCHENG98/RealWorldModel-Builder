module ModelBuilder where

import CSVParser
import Numeric.GSL.Fitting
import Model 
import Plot
import Data.List
import Data.Char

go :: IO ()
go =
   do 
    putStrLn "Welcome to Real-World Model Builder."
    putStrLn "Please enter the filename of the CSV file you want to model."
    filename <- getLineFixed
    rdat <- readcsv filename
    model <- selectModel
    (abstol,reltol,numiter,guess) <- askreq (numPara model) 
    let (sol,path) = fitModelScaled abstol reltol numiter (function model, functionDer model) dat guess
    draw dat model (map fst sol)
    putStrLn "These are the parameters."
    report1by1 sol 1 
    let msg = "The chi2 of the fitting is "++(show (chi2 (function model (map fst sol)) dat (numPara model)))++"."
    putStrLn msg
    

-- create a new model based on the users choice of basic models
selectModel :: IO Model
selectModel = 
    do
        putStrLn "There are 8 types of basics models: \n 1. Linear Model \n 2. Quadratic Model\n 3. Cubic Model\n 4. Fourth Power Model\n 5. Trig Model\n 6. Log Model\n 7. Hyperbolic Sine Model\n 8. Exponential Model"
        putStrLn "Please type in the numbers of the model you want to combine, sperated by commas."
        order <- getLineFixed 
        let orderlist = (filter (all isDigit) (filter (/="") (rmvdup (splitsep (==',') order)))) 
        let model = modelCombine orderlist
        let reportmodel = "The combined model is " ++ name model
        putStrLn reportmodel
        return model 

-- remove duplicates in a list, from assignment 2        
rmvdup lst = [ h | (h:t) <- tails lst,  not (h `elem` t)]

-- combine a list of models  
modelCombine :: [[Char]] -> Model      
modelCombine [] = emptyModel
modelCombine (h:s)  
              |(read h::Int) <= 8 = combine (basicModelList !! ((read h::Int)-1)) (modelCombine s) 
              | otherwise = modelCombine s
                    
-- ask the requirement about the fitting
askreq :: Int -> IO (Double, Double, Int, [Double])
askreq n = 
    do
        abstol <- askabs
        reltol <- askrel
        numiter <- asknum
        guess <- askguess n
        return (abstol,reltol,numiter,guess)

-- ask the absolute about the fitting
askabs :: IO Double
askabs = 
    do 
        putStrLn "Please indicate the absolute tolerence."
        abs <- getLineFixed
        if (myisNumber abs) 
            then do
                    let abstol = read abs :: Double
                    return abstol
            else do
                    putStrLn "The input is not a valid data."
                    askabs

-- ask the relative about the fitting
askrel :: IO Double
askrel = 
    do 
        putStrLn "Please indicate the relative tolerence."
        rel <- getLineFixed
        if (myisNumber rel) 
            then do
                    let reltol = read rel :: Double
                    return reltol
            else do
                    putStrLn "The input is not a valid data."
                    askrel               

-- ask the maximale number of iterations
asknum :: IO Int
asknum = 
    do 
        putStrLn "Please indicate the maxmum number of iteration."
        num <- getLineFixed
        if (all isDigit num) 
            then do
                    let numiter = read num :: Int
                    return numiter
            else do
                    putStrLn "The input is not a valid data."
                    asknum                             

-- ask user the initial guess of the parameters
askguess :: Int -> IO [Double]
askguess n =
    do 
        putStrLn "Please indicate you guess of each parameter, sperated by commas."
        let msg = "You need "++(show n)++" parameters in total."
        putStrLn msg
        guessStr <- getLineFixed
        let guessL = (filter myisNumber (splitsep (==',') guessStr))
        if (length guessL) == n
            then do 
                    let guess =  map (\ x -> read x :: Double) guessL
                    return guess
            else askguess n

-- report parameters one by one
report1by1 :: (Num a1, Show a2, Show a1, Show a3) => [(a2, a3)] -> a1 -> IO ()
report1by1 [] _ = 
    do 
        putStrLn "These are the all of the paraters.\n"

report1by1 (h:s) n =
    do 
        let msgp = "The parameter "++(show n)++" is "++(show (fst h))++"."
        let msge =  "The error of parameter "++(show n)++" is "++(show (snd h))++"."
        putStrLn msgp
        putStrLn msge
        report1by1 s (n+1)


-- calculate chi2
chi2 :: ([Double] -> [Double]) -> [([Double], ([Double], Double))] -> Int -> Double        
chi2 func dat nump = (chi2sum func dat) / (fromIntegral ((length dat) - nump))

chi2sum :: ([Double]->[Double]) -> [([Double],([Double],Double))] -> Double 
chi2sum func [] = 0
chi2sum func (h:s) = ((head (func (fst h))) - (head (fst (snd h))))^2 + chi2sum func s


