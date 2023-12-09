getDWforPGM=function(model,data_){

#returns DW statistic based on residuals
#data needs to be a pdata object as does the model
resids=residuals(model)

#need this convoluted way because the plm library is poorly built around factors...
indexfitted=cbind(as.numeric(paste(index(model)[[1]])),
                  as.numeric(paste(index(model)[[2]])))  #this is an N(model) x 2 matrix with ID and TIMEID as columns

#index time and household
hh_ind = levels(as.factor(indexfitted[,1]))
dw_vec=rep(NA,length(hh_ind))

for(hh in 1:length(hh_ind)){
  resids_hh = resids[indexfitted[,1]==hh_ind[hh]]
  resids_lag = c(NA,resids_hh[-length(resids_hh)])
  dw_vec[hh] = sum((resids_hh - resids_lag)^2,na.rm=TRUE) / sum(resids_hh^2,na.rm=TRUE)
  }

return(dw_vec)
}