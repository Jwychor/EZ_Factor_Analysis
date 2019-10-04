# EZ-Factor-Analysis Dashboard
Interactive R - Shiny dashboard for factor analysis.


# Usage and Description
Near the top line, replace the string 'name' with the name of a dataframe in your global environment.
Then press ```cntrl``` + ```a```, then ```cntrl``` + ```enter```. The app should initialize.

The app includes 2 panels: In the factor panel, there is a Scree-plot, a sliding bar indicating the number of factors to be used in a principal component analysis,
a sliding bar indicating what number to hide loadings at, and a choice between a "varimax" and "oblimin" rotated principal 
component analysis, scale Cronbach's Alpha, and scale item statistics.

In the subscales tab, there is sliding bar that indicates which subscale to be analyzed, a subscale Cronbach's Alpha, Alpha-if-Item-Dropped for each item in the subscale, and item statistics for each item in the subscale.

# Dependencies
Functions from the following packages are required to be installed on your machine before this code will run:

dplyr
DT
jmv
psych
shiny
shinydashboard
ggplot2
ggcorrplot
Hmisc
