module Model where

data Model = Model {   name :: String
                    ,  numPara :: Int
                    ,  function :: [Double] -> [Double] -> [Double]
                    ,  functionDer :: [Double] -> [Double] -> [[Double]] 
                    }  


-- emptymodel
emptyFunc [] [x] = [0]
emptyFuncDer [] [x] = [[]]

emptyModel = Model "" 0 emptyFunc emptyFuncDer

-- linear model
lineFunc [a,b] [x] = [a * x + b]
lineFuncDer [a,b] [x] = [[x,1]]

lineModel = Model "linear: ax+b" 2 lineFunc lineFuncDer

-- quadratic model
quadFunc [a,b,c] [x] = [a * x * x + b * x + c]
quadFuncDer [a,b,c] [x] = [[x^2, x , 1]]

quadModel = Model "quadratic: ax^2+bx+c" 3 quadFunc quadFuncDer 

-- cubic model
cubFunc [a,b,c,d] [x] = [a*x^3 + b*x^2 + c*x +d]
cubFuncDer [a,b,c,d] [x] = [[3*x^2, 2*x, x, 1]]

cubModel = Model "cubic: ax^3+bx^2+cx+d" 4 cubFunc cubFuncDer

-- fourth pow model
fourthPowFunc [a,b,c,d,e] [x] = [a*x^4 + b*x^3 + c*x^2 + d*x + e]
fourthPowFuncDer [a,b,c,d,e] [x] = [[4*x^3, 3*x^2, 2*x, x, 1]] 

fourthPowModel = Model "fourth power: ax^4+bx^3+cx^2+dx+e" 5 fourthPowFunc fourthPowFuncDer

-- trig model
tirgFunc [a,b,c,d] [x] = [a*sin(b*x)+c*cos(d*x)]
trigFuncDer [a,b,c,d] [x] = [[sin(b*x),a*x*cos(b*x),cos(d*x),-1*c*x*sin(d*x)]]

trigModel = Model "trig: a*sin(b*x)+c*cos(d*x)" 4 tirgFunc trigFuncDer

-- log model
logFunc [a,b] [x] = [a*log(b*x)]
logFuncDer [a,b] [x] = [[log(b*x),a/b]]

logModel = Model "log: a*log(b*x)" 2 logFunc logFuncDer

-- hyperbolic sin model
sinhFunc [a,b] [x] = [a*sinh(b*x)]
sinhFuncDer [a,b] [x] = [[sinh(b*x),a*x*cosh(b*x)]]

sinhModel = Model "sinh: a*sinh(b*x)" 2 sinhFunc sinhFuncDer

-- exponential model
expFunc [a,lambda,b] [t] = [a * exp (-lambda * t) + b]
expFuncDer [a,lambda,b] [t] = [[exp (-lambda * t), -t * a * exp(-lambda*t) , 1]]

expModel  = Model "exponential: a*exp(-lambda*t)+b" 3 expFunc expFuncDer

basicModelList = [lineModel,quadModel,cubModel,fourthPowModel,trigModel,logModel,sinhModel,expModel]


combine :: Model -> Model -> Model 
combine m1 m2 = Model newname newnum newfunc newfuncder
     where newname = name m1 ++ " & " ++ name m2
           newnum = numPara m1 + numPara m2 
           newfunc = funcComb (numPara m1) (numPara m2) (function m1) (function m2)
           newfuncder = funcDerComb (numPara m1) (numPara m2) (functionDer m1) (functionDer m2)

funcComb :: Int 
         -> Int 
         -> ([Double] -> [Double] -> [Double]) 
         -> ([Double] -> [Double] -> [Double]) 
         -> ([Double] -> [Double] -> [Double])
(funcComb n m f g) lsta [x] = [(head (f (take n lsta) [x])) + (head (g (take m (drop n lsta)) [x]))] 

funcDerComb :: Int 
         -> Int 
         -> ([Double] -> [Double] -> [[Double]]) 
         -> ([Double] -> [Double] -> [[Double]]) 
         -> ([Double] -> [Double] -> [[Double]])
(funcDerComb n m f g) lsta [x] = [(head (f (take n lsta) [x])) ++ (head (g (take m (drop n lsta)) [x]))] 

