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
colnames(outmat)=c("CONSUMPTION","CONSUMPTION","DISCRETIONARY CONSUMPTION","DISCRETIONARY CONSUMPTION")

#number of HH
outmat[10,]=c(ncol(model_list[[1]]$indcoef),ncol(model_list[[2]]$indcoef),ncol(model_list[[3]]$indcoef),ncol(model_list[[4]]$indcoef))
#total # of observations
outmat[12,]=c(length(model_list[[1]]$fitted.values),length(model_list[[2]]$fitted.values),length(model_list[[3]]$fitted.values),length(model_list[[4]]$fitted.values))

#M5 - jones asymmetric CONS
tab=round(summary(model_list[[1]])$CoefTable,3)
if(intercept==FALSE){
  tab=rbind(rep(NA,ncol(tab)),tab)
  }
outmat[1,1]=paste(tab[2,1],ifelse(tab[2,4]>0.1,"",ifelse(tab[2,4]>0.05,"*",ifelse(tab[2,4]>0.01,"**","***"))),sep="")
outmat[2,1]=tab[2,2]
outmat[3,1]=paste(tab[3,1],ifelse(tab[3,4]>0.1,"",ifelse(tab[3,4]>0.05,"*",ifelse(tab[3,4]>0.01,"**","***"))),sep="")
outmat[4,1]=tab[3,2]
outmat[6:7,1]=c("No","No")
outmat[9,1]=round(summary(model_list[[1]])$rsqr,2)
outmat[11,1]=20
outmat[13,1]= round(pbnftest(model_list[[1]])$statistic,2)
outmat[14,1]= round(pbnftest(model_list[[1]],test="lbi")$statistic,2)
outmat[15,1]= round(pbnftest(model_list[[1]],test="bnf")$statistic,2)

#M5 - jones asymmetric DISCRETIONARY CONS
tab=round(summary(model_list[[2]])$CoefTable,3)
if(intercept==FALSE){
  tab=rbind(rep(NA,ncol(tab)),tab)
  }
outmat[1,2]=paste(tab[2,1],ifelse(tab[2,4]>0.1,"",ifelse(tab[2,4]>0.05,"*",ifelse(tab[2,4]>0.01,"**","***"))),sep="")
outmat[2,2]=tab[2,2]
outmat[3,2]=paste(tab[3,1],ifelse(tab[3,4]>0.1,"",ifelse(tab[3,4]>0.05,"*",ifelse(tab[3,4]>0.01,"**","***"))),sep="")
outmat[4,2]=tab[3,2]
outmat[6:7,2]=c("No","No")
outmat[9,2]=round(summary(model_list[[2]])$rsqr,2)
outmat[11,2]=20
outmat[13,2]= round(pbnftest(model_list[[2]])$statistic,2)
outmat[14,2]= round(pbnftest(model_list[[2]],test="lbi")$statistic,2)
outmat[15,2]= round(pbnftest(model_list[[2]],test="bnf")$statistic,2)

#M8 - rot asymmetric with habits and jones CONS
tab=round(summary(model_list[[3]])$CoefTable,3)
if(intercept==FALSE){
  tab=rbind(rep(NA,ncol(tab)),tab)
  }
outmat[1,3]=paste(tab[2,1],ifelse(tab[2,4]>0.1,"",ifelse(tab[2,4]>0.05,"*",ifelse(tab[2,4]>0.01,"**","***"))),sep="")
outmat[2,3]=tab[2,2]
outmat[3,3]=paste(tab[3,1],ifelse(tab[3,4]>0.1,"",ifelse(tab[3,4]>0.05,"*",ifelse(tab[3,4]>0.01,"**","***"))),sep="")
outmat[4,3]=tab[3,2]
outmat[6:7,3]=c("Yes","Yes")
outmat[9,3]=round(summary(model_list[[3]])$rsqr,2)
outmat[11,3]=20
outmat[13,3]= round(pbnftest(model_list[[3]])$statistic,2)
outmat[14,3]= round(pbnftest(model_list[[3]],test="lbi")$statistic,2)
outmat[15,3]= round(pbnftest(model_list[[3]],test="bnf")$statistic,2)

#M8 - rot asymmetric with habits and jones DISCRETIONARY CONS
tab=round(summary(model_list[[4]])$CoefTable,3)
if(intercept==FALSE){
  tab=rbind(rep(NA,ncol(tab)),tab)
  }
outmat[1,4]=paste(tab[2,1],ifelse(tab[2,4]>0.1,"",ifelse(tab[2,4]>0.05,"*",ifelse(tab[2,4]>0.01,"**","***"))),sep="")
outmat[2,4]=tab[2,2]
outmat[3,4]=paste(tab[3,1],ifelse(tab[3,4]>0.1,"",ifelse(tab[3,4]>0.05,"*",ifelse(tab[3,4]>0.01,"**","***"))),sep="")
outmat[4,4]=tab[3,2]
outmat[6:7,4]=c("Yes","Yes")
outmat[9,4]=round(summary(model_list[[4]])$rsqr,2)
outmat[11,4]=20
outmat[13,4]= round(pbnftest(model_list[[4]])$statistic,2)
outmat[14,4]= round(pbnftest(model_list[[4]],test="lbi")$statistic,2)
outmat[15,4]= round(pbnftest(model_list[[4]],test="bnf")$statistic,2)

return(outmat)
}









