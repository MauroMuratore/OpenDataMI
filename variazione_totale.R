library(quantmod)
f_delt_ref <- function(dataframe){
  ref_anno <- dataframe[1]
  riferimento <- rep(ref_anno, length(dataframe))
  variazione <- Delt(riferimento, dataframe)*-100
  
  return(variazione)
}

f_delt_prev <- function(dataframe){
  variazione <- Delt(dataframe)*-100
  variazione[1] <- 0
  return(variazione)
}