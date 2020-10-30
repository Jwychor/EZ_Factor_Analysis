library('datasets')
library('rsconnect')
irisDF <- iris
attitudeDF <- attitude
if(!'remotes' %in% installed.packages()){
  install.packages('remotes', dependencies = T)
}
if(!'rlang' %in% installed.packages()){
  remotes::install_github('r-lib/rlang', dependencies = T)
}
library('remotes')
remotes::install_github("Jwychor/EZ_Factor_Analysis", dependencies = T, update = "ask", force=T)
library('EZFA')

EZ_FA()
