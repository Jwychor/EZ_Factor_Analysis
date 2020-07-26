# EZ Factor Analysis Dashboard
Interactive R - Shiny dashboard for factor analysis, subscale reliability, linear regression, and correlational analysis.

# Important
Only works in R versions >= 3.6.2.

# Installation and Usage
The following code will install and instiatiate the dashboard in R Studio. 
```R
if(!'remotes' %in% installed.packages()){
  install.packages('remotes', dependencies = T)
}
if(!'rlang' %in% installed.packages()){
  remotes::install_github('r-lib/rlang', dependencies = T)
}
library('remotes')
remotes::install_github("Jwychor/EZ_Factor_Analysis", dependencies = T, update = "ask")
library('EZFA')

EZ_FA()
```
If the console prompts you to install new versions of dependencies that are already installed, this is not necessary and entering "3" (none) when prompted is fine.

The dashboard can access all dataframes in the current global environment. Closing the application will allow the user to access R functionality again. To use the dashboard in future files use 
```
library(EZFA)

``` 
to add the package to the required libraries and
```
EZ_FA()
````
to open the dashboard.

### NOTE: 
Non-numeric columns in data frames will be ignored. Errors and warnings thrown during use will not stop the application. These will be displayed in the R Studio console.

## Factor Analysis
The app includes 3 panels: In the factor panel, there is a Scree-plot, a sliding bar indicating the number of factors to be used in a principal component analysis, a sliding bar indicating what number to hide loadings at, and a choice between a "varimax" and "oblimin" rotated principal component analysis, scale Cronbach's Alpha, and scale item statistics. Finally, if the 1st column in the dataframe is a dependent variable, it can be excluded from the analysis with the ```First Column is DV?``` box changed to "Yes".

![image](https://github.com/Jwychor/EZ_Factor_Analysis/blob/master/Images/EZ_FA%20Page%201.JPG)

## Subscales
In the subscales tab, there is sliding bar that indicates which subscale to be analyzed, a subscale Cronbach's Alpha, Alpha-if-Item-Dropped for each item in the subscale, and item statistics for each item in the subscale. Items for a particular subscale are also displayed below the dial controlling which subscale to analyze and can be changed dynamically for easy copying and pasting.

![image](https://github.com/Jwychor/EZ_Factor_Analysis/blob/master/Images/EZ_FA%20Page%202.JPG)

## Regression
In the regression tab, there is a list of each variable in the selected dataframe. Any number of variables can be checked as an independent variable (IV), and one variable will be selected as the dependent variable (DV). The ```interactions``` button specifies whether interactions between all terms should be tested in the regression model or not. A correlation matrix can be found to help in deciding the IVs and DVs. Regressions can be specified to be standardized (z-scores) or unstandardized (raw scores). The regression print out after pressing ```run``` will contain beta-weights if standardized or B-weights if unstandardized under the ```estimates``` column with the ```(intercept)``` representing the y-intercept. R-squared and F values are also provided below the table.

![image](https://github.com/Jwychor/EZ_Factor_Analysis/blob/master/Images/EZ_FA%20Page%203.JPG)

# Dependencies
Functions from the following packages are required to be installed and required on your machine before this code will run:
```
Rlang
DT
devtools
jmv
tidyverse
psych
shiny
ggcorrplot

```
