# EZ Factor Analysis Dashboard
Interactive R - Shiny dashboard for factor analysis, subscale reliability, linear regression, and correlational analysis.

# Important
Only works in R version >= 3.6.2.
Dataframes to be analyzed must be in the global environment prior to startup.

# Usage and Description
The following code will install and instiatiate the dashboard. 
```R
if(!'rlang' %in% installed.packages()){
  install.packages('rlang',dependencies = T)
}
if(!'devtools' %in% installed.packages()){
  install.packages('devtools',dependencies = T)
}
library('devtools')
install_github("Jwychor/EZ_Factor_Analysis")
packs<-c('shiny','rlang','dplyr','DT','jmv','shiny','ggplot2','ggcorrplot','psych','EZFA')
sapply(packs,library,character = T)
EZ_FA()
```
The dashboard can access all dataframes in the current global environment. Closing the application will allow the user to access R functionality again. To open the dashboard, execute ```EZ_FA()``` again.

### NOTE: 
Non-numeric columns in data frames will be ignored. Errors and warnings thrown during use will not stop the application. These will be displayed in the R Studio console.

## Factor Analysis
The app includes 3 panels: In the factor panel, there is a Scree-plot, a sliding bar indicating the number of factors to be used in a principal component analysis, a sliding bar indicating what number to hide loadings at, and a choice between a "varimax" and "oblimin" rotated principal component analysis, scale Cronbach's Alpha, and scale item statistics. Finally, if the 1st column in the dataframe is a dependent variable, it can be excluded from the analysis with the ```First Column is DV?``` box changed to "Yes".

## Subscales
In the subscales tab, there is sliding bar that indicates which subscale to be analyzed, a subscale Cronbach's Alpha, Alpha-if-Item-Dropped for each item in the subscale, and item statistics for each item in the subscale. Items for a particular subscale are also displayed below the dial controlling which subscale to analyze and can be changed dynamically for easy copying and pasting.

## Regression
In the regression tab, there is a list of each variable in the selected dataframe. Any number of variables can be checked as an independent variable (IV), and one variable will be selected as the dependent variable (DV). The ```interactions``` button specifies whether interactions between all terms should be tested in the regression model or not. A correlation matrix can be found to help in deciding the IVs and DVs. Regressions can be specified to be standardized (z-scores) or unstandardized (raw scores). The regression print out after pressing ```run``` will contain beta-weights if standardized or B-weights if unstandardized under the ```estimates``` column with the ```(intercept)``` representing the y-intercept. R-squared and F values are also provided below the table.


# Dependencies
Functions from the following packages are required to be installed and required on your machine before this code will run:
```
dplyr
DT
devtools
ggplot2
ggcorrplot
jmv
psych
Rlang
shiny
```
