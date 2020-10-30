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
devtools::install_github("Jwychor/EZ_Factor_Analysis", dependencies = T, update = "ask")
library('EZFA')
EZ_FA()
invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
install.packages('rlang')
