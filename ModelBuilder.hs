module ModelBuider where

import CSVParser
import Numeric.GSL.Fitting
import Model 
import Plot
import Data.List
import Data.Char

go =
   do 
    putStrLn "Welcome to Real-World Model Builder."
    putStrLn "Please enter the filename of the CSV file you want to model."
    filename <- getLine 
    dat <- readcsv filename
    model <- selectModel
    let reportmodel = "The combined model is" ++ name model
    putStrLn reportmodel
    putStrLn "Please indicate the absolute tolerence."
    abs <- getLine
    let abstol = read abs :: Double
    putStrLn "Please indicate the relative tolerence."
    rel <- getLine
    let reltol = read rel :: Double
    putStrLn "Please indicate the maximum iteration."
    ite <- getLine
    let numiter = read ite :: Int
    putStrLn "Please indicate you guess of each parameter, sperated by commas."
    guessStr <- getLine
    let guessL = splitsep (==',') guessStr 
    let guess =  map (\ x -> read x :: Double) guessL
    let (sol,path) = fitModelScaled abstol reltol numiter (function model, functionDer model) dat guess
    draw dat model (map fst sol)
    return sol 



selectModel = 
    do
        putStrLn "There are 8 types of basics models: \n 1. Linear Model \n 2. Quadratic Model\n 3. Cubic Model\n 4. Fourth Power Model\n 5. Trig Model\n 6. Log Model\n 7. Hyperbolic Sine Model\n 8. Exponential Model"
        putStrLn "Please type in the numbers of the model you want to combine, sperated by commas."
        order <- getLine 
        let orderlist = splitsep (==',') order 
        let model = modelCombine orderlist
        return model 

modelCombine [] = emptyModel
modelCombine (h:s) = combine (basicModelList !! ((digitToInt (head h))-1)) (modelCombine s)        


