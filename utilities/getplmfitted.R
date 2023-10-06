getplmfitted=function(model,data_){

#returns fitted values INCLUDING NA because plm is fucking shit and doesnt allow this
#data needs to be a pdata object as does the model

fittedvals=fitted(model)
resids=residuals(model)

#need this convoluted way because the plm library is poorly built around factors...
indexfitted=cbind(as.numeric(paste(index(model)[[1]])),
                  as.numeric(paste(index(model)[[2]])))  #this is an N(model) x 2 matrix with ID and TIMEID as columns

indexdata=cbind(as.numeric(paste(index(data_)[[1]])),
                as.numeric(paste(index(data_)[[2]])))


#create fitted vals and residuals series:

fitvec=rep(NA,nrow(data_))
residvec=rep(NA,nrow(data_))

for(i in 1:nrow(indexfitted)){
  ind=which(indexdata[,1]==indexfitted[i,1] & indexdata[,2]==indexfitted[i,2])
  fitvec[ind]=fittedvals[i]
  residvec[ind]=resids[i]
  }


return(list(fitvec,residvec))
}