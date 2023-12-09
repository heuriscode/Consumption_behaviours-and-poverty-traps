#F-test
getFSTATforPGM=function(model_restrict,model_full,data_){

#data needs to be a pdata object as does the model
RSS1=residuals(model_restrict)^2
RSS2=residuals(model_full)^2

#need this convoluted way because the plm library is poorly built around factors...
indexfitted=cbind(as.numeric(paste(index(model_restrict)[[1]])),
                  as.numeric(paste(index(model_restrict)[[2]])))  #this is an N(model) x 2 matrix with ID and TIMEID as columns

#index time and household
hh_ind = levels(as.factor(indexfitted[,1]))
dw_vec=rep(NA,length(hh_ind))
k1 = length(model_restrict$coef)
k2 = length(model_full$coef)
n = length(levels(as.factor(indexfitted[,2])))  #weeks per hh

fstat=rep(NA,length(hh_ind))
for(hh in 1:length(hh_ind)){
  RSS1_hh = RSS1[indexfitted[,1]==hh_ind[hh]]
  RSS2_hh = RSS2[indexfitted[,1]==hh_ind[hh]]
  fstat[hh] = ((sum(RSS1_hh,na.rm=TRUE) - sum(RSS2_hh,na.rm=TRUE))/(k2-k1)) / (sum(RSS2_hh,na.rm=TRUE)/(n-k2))
  }

return(fstat)
}

