get_outmat_JONES=function(model_list,intercept=FALSE){


#ROT
outmat=matrix(NA,nrow=15,ncol=4)
rownames(outmat)=c("difference from Mean Parish Consumption)","Std. Error",
                   "difference from Mean Parish Consumption) (+ve values)","Std. Error",
                   "", #gap
                   "Include controls for RoT?","Include controls for Habits?",
                   "", #gap
                   "R-squared","Number households","Number time periods (used)","Total used observations",
                   "Durbin-Watson (DW) Statistic","Baltagi-Wu LBI statistic","Barghava et al Durbin Watson Statistic")
colnames(outmat)=c("CONSUMPTION","CONSUMPTION","DISCRETIONARY CONSUMPTION","DISCRETIONARY CONSUMPTION","FLEXIBLE CONSUMPTION","FLEXIBLE CONSUMPTION")


for(mmm in 1:length(model_list)){

  #get coefficients
  tab=round(summary(model_list[[mmm]])$CoefTable,3)

  #include intercept or not?
  if(intercept==FALSE){
    tab=rbind(rep(NA,ncol(tab)),tab)
    }
  
  #fill coefficients
  for(rrr in 1:2){
    outmat[((rrr-1)*2+1),mmm]=paste(tab[((rrr-1)*2+2),1],ifelse(tab[((rrr-1)*2+2),4]>0.1,"",ifelse(tab[((rrr-1)*2+2),4]>0.05,"*",ifelse(tab[((rrr-1)*2+2),4]>0.01,"**","***"))),sep="")
    outmat[((rrr-1)*2+2),mmm]=tab[2,2]
  }

  #Add comments on model type (i.e. including the other behavioural functions or not):
  if((mmm %% 2) == 0) {
    outmat[6:7,mmm] = c("Yes","Yes")
  } else {
    outmat[6:7,mmm] = c("No","No")
  }

  #basic model statistics (r-squared, num hh, num time periods, total obs)
  outmat[9,mmm]=c(round(summary(model_list[[mmm]])$rsqr,2))
  outmat[10,mmm]=dim(model_list[[mmm]]$indcoef)[2],                                     #num households
  outmat[11,mmm]=length(model_list[[mmm]]$fitted.values)/(dim(model_list[[mmm]]$indcoef)[2]),  #time periods used
  outmat[12,mmm]=length(model_list[[mmm]]$fitted.values),
  
  #DW and other test statistics
  outmat[13,mmm]= round(pbnftest(model_list[[mmm]])$statistic,2)
  outmat[14,mmm]= round(pbnftest(model_list[[mmm]],test="lbi")$statistic,2)
  outmat[15,mmm]= round(pbnftest(model_list[[mmm]],test="bnf")$statistic,2)

}

return(outmat)
}









