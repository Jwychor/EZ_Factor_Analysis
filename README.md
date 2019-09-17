# EZ-Factor-Analysis
Interactive R - Shiny dashboard for factor analysis.


# Usage and Description
Near the top line, replace the string 'name' with the name of a dataframe in your global environment.
Then press ```cntrl``` + ```a```, then ```cntrl``` + ```enter```. The app should initialize.

The app includes a Scree-plot, a sliding bar indicating the number of factors to be used in a principal component analysis,
a sliding bar indicating what number to hide loadings at, and a choice between a "varimax" and "oblimin" rotated principal 
component analysis.


# Dependencies
Functions from the following packages are required to be installed on your machine before this code will run:

dplyr
DT
jmv
shiny
shinydashboard
