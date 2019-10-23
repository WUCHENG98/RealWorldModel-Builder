# RealWorldModel-Builder
This project is aimed to build models based on the data from the real world,  i.e. the data of independent variable, dependent variable and the uncertainty of dependent variable.

## User Manual
1. In ghci, load `ModelBuilder` and enter `go`.

2. Enter the filename of the CSV file you want to analyze. If you make a mistake, you could use `DELETE` key to change. If you enter the empty string, you will be asked to enter it again. The validity of the file would be checked. There should be no string; there should be only three colums and the the number of rows should be the same; the minimum of 10 datapoints are required.  

3. Enter the number seperated with commas to combine a new model based on the 8 basic models. If you make a mistake, you could use `DELETE` key to change. If you enter the empty string, you will be asked to enter it again. The duplicated model would be removed and the number above 8 would be ignored. 

4. Enter the absolute and relative tolerance and number of maximum iteration. If you make a mistake, you could use `DELETE` key to change. If you enter the empty string, you will be asked to enter it again. If your input is not a string, you would be asked to input it again.

5. Enter your guess of the parameters in the model. If you make a mistake, you could use `DELETE` key to change. If you enter the empty string, you will be asked to enter it again. If the entered parameters is not enough, you would be asked to enter it again.

6. The fitting would start. The result would be the value and the error of the parameters. To help you evaluate the fittin quality, the fitting diagram and the residual diagram would be plotted and saved. Also the chi-square would be calculated.

## Contributor
Yanbai Chen: `CSVParser`

Yunan Xu: `Plot`

Wucheng Zhang: `Model`, `ModelBuilder`

