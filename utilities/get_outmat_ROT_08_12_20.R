get_outmat_ROT=function(model_list,intercept=FALSE){


#ROT
outmat=matrix(NA,nrow=18,ncol=4)
rownames(outmat)=c("diff Exp. Income","Std. Error",
                       "diff Exp. Income (+ve values)","Std. Error",
                       "diff Unexpected Income","Std. Error",
                       "diff Unexpected Income (+ve values)","Std. Error",
                       "", #gap
                       "Include controls for Habits?","Include controls for 'Keeping Up'?",
                       "", #gap
                       "R-squared","Number households","Number time periods (used)","Total used observations",
                       "Baltagi-Wu LBI statistic","Barghava et al Durbin Watson Statistic")
colnames(outmat)=c("CONSUMPTION","CONSUMPTION","DISCRETIONARY CONSUMPTION","DISCRETIONARY CONSUMPTION")

#number of HH
outmat[14,]=c(ncol(model_list[[1]]$indcoef),ncol(model_list[[2]]$indcoef),ncol(model_list[[3]]$indcoef),ncol(model_list[[4]]$indcoef))
#total # of observations
outmat[16,]=c(length(model_list[[1]]$fitted.values),length(model_list[[2]]$fitted.values),length(model_list[[3]]$fitted.values),length(model_list[[4]]$fitted.values))


#M5 - asymmetric CONS
tab=round(summary(model_list[[1]])$CoefTable,3)
if(intercept==FALSE){
  tab=rbind(rep(NA,ncol(tab)),tab)
  }
outmat[1,1]=paste(tab[2,1],ifelse(tab[2,4]>0.1,"",ifelse(tab[2,4]>0.05,"*",ifelse(tab[2,4]>0.01,"**","***"))),sep="")
outmat[2,1]=tab[2,2]
outmat[3,1]=paste(tab[3,1],ifelse(tab[3,4]>0.1,"",ifelse(tab[3,4]>0.05,"*",ifelse(tab[3,4]>0.01,"**","***"))),sep="")
outmat[4,1]=tab[3,2]
outmat[5,1]=paste(tab[4,1],ifelse(tab[4,4]>0.1,"",ifelse(tab[4,4]>0.05,"*",ifelse(tab[4,4]>0.01,"**","***"))),sep="")
outmat[6,1]=tab[4,2]
outmat[7,1]=paste(tab[5,1],ifelse(tab[5,4]>0.1,"",ifelse(tab[5,4]>0.05,"*",ifelse(tab[5,4]>0.01,"**","***"))),sep="")
outmat[8,1]=tab[5,2]
outmat[10:11,1]=c("No","No")
outmat[13,1]=c(round(summary(model_list[[1]])$rsqr,2))
outmat[15,1]=20
outmat[17,1]= round(pbnftest(model_list[[1]],test="lbi")$statistic,2)
outmat[18,1]= round(pbnftest(model_list[[1]],test="bnf")$statistic,2)

#M5 - asymmetric DISCRETIONARY CONS
tab=round(summary(model_list[[2]])$CoefTable,3)
if(intercept==FALSE){
  tab=rbind(rep(NA,ncol(tab)),tab)
  }
outmat[1,2]=paste(tab[2,1],ifelse(tab[2,4]>0.1,"",ifelse(tab[2,4]>0.05,"*",ifelse(tab[2,4]>0.01,"**","***"))),sep="")
outmat[2,2]=tab[2,2]
outmat[3,2]=paste(tab[3,1],ifelse(tab[3,4]>0.1,"",ifelse(tab[3,4]>0.05,"*",ifelse(tab[3,4]>0.01,"**","***"))),sep="")
outmat[4,2]=tab[3,2]
outmat[5,2]=paste(tab[4,1],ifelse(tab[4,4]>0.1,"",ifelse(tab[4,4]>0.05,"*",ifelse(tab[4,4]>0.01,"**","***"))),sep="")
outmat[6,2]=tab[4,2]
outmat[7,2]=paste(tab[5,1],ifelse(tab[5,4]>0.1,"",ifelse(tab[5,4]>0.05,"*",ifelse(tab[5,4]>0.01,"**","***"))),sep="")
outmat[8,2]=tab[5,2]
outmat[10:11,2]=c("No","No")
outmat[13,2]=c(round(summary(model_list[[2]])$rsqr,2))
outmat[15,2]=20
outmat[17,2]= round(pbnftest(model_list[[2]],test="lbi")$statistic,2)
outmat[18,2]= round(pbnftest(model_list[[2]],test="bnf")$statistic,2)

#M8 - rot asymmetric with habits and jones CONS
tab=round(summary(model_list[[3]])$CoefTable,3)
if(intercept==FALSE){
  tab=rbind(rep(NA,ncol(tab)),tab)
  }
outmat[1,3]=paste(tab[2,1],ifelse(tab[2,4]>0.1,"",ifelse(tab[2,4]>0.05,"*",ifelse(tab[2,4]>0.01,"**","***"))),sep="")
outmat[2,3]=tab[2,2]
outmat[3,3]=paste(tab[3,1],ifelse(tab[3,4]>0.1,"",ifelse(tab[3,4]>0.05,"*",ifelse(tab[3,4]>0.01,"**","***"))),sep="")
outmat[4,3]=tab[3,2]
outmat[5,3]=paste(tab[4,1],ifelse(tab[4,4]>0.1,"",ifelse(tab[4,4]>0.05,"*",ifelse(tab[4,4]>0.01,"**","***"))),sep="")
outmat[6,3]=tab[4,2]
outmat[7,3]=paste(tab[5,1],ifelse(tab[5,4]>0.1,"",ifelse(tab[5,4]>0.05,"*",ifelse(tab[5,4]>0.01,"**","***"))),sep="")
outmat[8,3]=tab[5,2]
outmat[10:11,3]=c("Yes","Yes")
outmat[13,3]=c(round(summary(model_list[[3]])$rsqr,2))
outmat[15,3]=20
outmat[17,3]= round(pbnftest(model_list[[3]],test="lbi")$statistic,2)
outmat[18,3]= round(pbnftest(model_list[[3]],test="bnf")$statistic,2)

#M8 - rot asymmetric with habits and jones DISCRETIONARY CONS
tab=round(summary(model_list[[4]])$CoefTable,3)
if(intercept==FALSE){
  tab=rbind(rep(NA,ncol(tab)),tab)
  }
outmat[1,4]=paste(tab[2,1],ifelse(tab[2,4]>0.1,"",ifelse(tab[2,4]>0.05,"*",ifelse(tab[2,4]>0.01,"**","***"))),sep="")
outmat[2,4]=tab[2,2]
outmat[3,4]=paste(tab[3,1],ifelse(tab[3,4]>0.1,"",ifelse(tab[3,4]>0.05,"*",ifelse(tab[3,4]>0.01,"**","***"))),sep="")
outmat[4,4]=tab[3,2]
outmat[5,4]=paste(tab[4,1],ifelse(tab[4,4]>0.1,"",ifelse(tab[4,4]>0.05,"*",ifelse(tab[4,4]>0.01,"**","***"))),sep="")
outmat[6,4]=tab[4,2]
outmat[7,4]=paste(tab[5,1],ifelse(tab[5,4]>0.1,"",ifelse(tab[5,4]>0.05,"*",ifelse(tab[5,4]>0.01,"**","***"))),sep="")
outmat[8,4]=tab[5,2]
outmat[10:11,4]=c("Yes","Yes")
outmat[13,4]=c(round(summary(model_list[[4]])$rsqr,2))
outmat[15,4]=20
outmat[17,4]= round(pbnftest(model_list[[3]],test="lbi")$statistic,2)
outmat[18,4]= round(pbnftest(model_list[[3]],test="bnf")$statistic,2)


return(outmat)
}









