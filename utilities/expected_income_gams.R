get_gams = function(){

    require(mgcv)

    #need to create a normal dataframe as mgcv does not accept pdata frames... hum.
    dfts = as.data.frame(pdat)
    #data frame is entirely orderded by ID-WEEK (ID1 with weeks 1-24, then ID2 with weeks 1-24, etc.)
    #so can do a simple loop to lag vars for each ID then do that recursively to get second lag. 

    lagfunc = function(x){
        return(c(NA,x[1:(length(x)-1)]))
    }

    # initialise series
    INC_L1 = INC_L2 = CONS_L1 = CONS_L2 = DISC_TO_TOT_CONS_RAT_L1 = PARISH_INC_L1 = PARISH_INC_L2 = PARISH_CONS_L1 = PARISH_CONS_L2 = c()

    ID_levels = levels(dfts$ID)
    for(hh in 1:length(ID_levels)){
        id = ID_levels[hh]
        subdat = dfts[dfts$ID==id,]
            
            #get series first
            INC_l1 = lagfunc(subdat$INC)
            INC_l2 = lagfunc(INC_l1)
            CONS_l1 = lagfunc(subdat$CONS)
            CONS_l2 = lagfunc(CONS_l1)
            DISC_TO_TOT_CONS_RAT_l1 = lagfunc(subdat$DISC_TO_TOT_CONS_RAT)
            #DISC_TO_TOT_CONS_RAT_l2 = lagfunc(DISC_TO_TOT_CONS_RAT_l1)
            PARISH_INC_l1 = lagfunc(subdat$PARISH_INC)
            PARISH_INC_l2 = lagfunc(PARISH_INC_l1)
            PARISH_CONS_l1 = lagfunc(subdat$PARISH_CONS)
            PARISH_CONS_l2 = lagfunc(PARISH_CONS_l1)

            #then add to vectors
            INC_L1 = c(INC_L1,INC_l1)
            INC_L2 = c(INC_L2,INC_l2)
            CONS_L1 = c(CONS_L1,CONS_l1)
            CONS_L2 = c(CONS_L2,CONS_l2)
            DISC_TO_TOT_CONS_RAT_L1 = c(DISC_TO_TOT_CONS_RAT_L1,DISC_TO_TOT_CONS_RAT_l1)
            #DISC_TO_TOT_CONS_RAT_L2 = c(DISC_TO_TOT_CONS_RAT_L2,DISC_TO_TOT_CONS_RAT_l2)
            PARISH_INC_L1 = c(PARISH_INC_L1,PARISH_INC_l1)
            PARISH_INC_L2 = c(PARISH_INC_L2,PARISH_INC_l2)
            PARISH_CONS_L1 = c(PARISH_CONS_L1,PARISH_CONS_l1)
            PARISH_CONS_L2 = c(PARISH_CONS_L2,PARISH_CONS_l2)
    }
    
    #Add new vars to dfts
    dfts = data.frame(
        dfts,
        "INC_L1" = INC_L1,
        "INC_L2" = INC_L2,
        "CONS_L1" = CONS_L1,
        "CONS_L2" = CONS_L2,
        "DISC_TO_TOT_CONS_RAT_L1" = DISC_TO_TOT_CONS_RAT_L1,
        #"DISC_TO_TOT_CONS_RAT_L2" = DISC_TO_TOT_CONS_RAT_L2,
        "PARISH_INC_L1" = PARISH_INC_L1,
        "PARISH_INC_L2" = PARISH_INC_L2,
        "PARISH_CONS_L1" = PARISH_CONS_L1,
        "PARISH_CONS_L2" = PARISH_CONS_L2,
        "WEEK_CONTINUOUS" = as.numeric(dfts$WEEK)
    )

    #Equation
    eq_exp_inc=INC~s(INC_L1) + 
                   s(INC_L2) +
                   #s(CONS_L1) + 
                   #s(CONS_L2) + 
                   s(PARISH_CONS_L1) +
                   s(PARISH_CONS_L2) +
                   s(WEEK_CONTINUOUS)

    #Loop through the sample and estimate individual models by ID.
    modelList = vector(mode="list",length=length(ID_levels))
    R2 = predinc = resids = errors = c()
    for(hh in 1:length(ID_levels)){
        id = ID_levels[hh]
        subdat = dfts[dfts$ID==id,]

        #estimate individual level models and save results
        init_res = try(gam(eq_exp_inc,data=subdat))
        if(class(init_res[[1]]) == "try-error"){
            errors=c(errors,hh)
            next()
        } else {
            modelList[[hh]] = init_res
            predinc = c(predinc,predict(modelList[[hh]]))
            resids = c(resids,modelList[[hh]]$residuals)
            R2[hh] = summary(modelList[[hh]])$dev
        }
        
        #leave space here for calculating durbin watson stats from the residuals - probably have to do manually
    }

    total_R2 = 1 - sum(resid^2,na.rm=TRUE) / suum((dfts$INC - mean(dfts$INC,na.rm=TRUE))^2,na.rm=TRUE)
    
    #generate results matrix
    len=length(predinc_reg$coef)
    predinc_mat=matrix(NA,nrow=(8+len),ncol=3)
    predinc_mat[1:len,]=round(summary(predinc_reg)$Coef[,-3],3)

    #manually enter values for R2, #hh, #time, #total, and DWtest because I'm feeling brain-lazy for thinking how to code this.
    predinc_mat[(len+2):(len+8),1]=c(0.54,423,22,9306,DWstat,BNF,LBI)


    ### CHECK AND REDO FOR THE FINAL REGRESSSION ###
    rownames(predinc_mat)=c("Intercept","L1(Income)","L2(Income)",
                            "L1(Consumption)","L2(Consumption)",
                            "L1 Own Discretionary/Total Consumption Ratio",
                            "L1(Parish mean income)","L2(Parish mean income)",
                            "L1(Parish mean consumption)","L2(Parish mean consumption)",
                            "",
                            "R-squared","# households","Time periods used","Total # observations",
                            "DW statistic","Barghava et al Durbin Watson Statistic","Baltagi-Wu LBI statistic")

    colnames(predinc_mat)=c("Estimate","Standard Error","P-value")

    write.table(predinc_mat,"results\\predicted income regression.csv",sep=",")

    #get fitted values and residuals
    fittedlist=getplmfitted(predinc_reg,pdat)
    EXPECT_INC=fittedlist[[1]]  #this represents the predictable portion of income
    RESID_INC=fittedlist[[2]]   #this represents the unexpected portion of income CALCULATED AS OBSERVED MINUS EXPECTED!!!

    #get differences for the pos components manually due to plm limitations
    dPOS_EXPINC=dPOS_UNEXPINC=rep(NA,nrow(dat))
    for(i in 1:nrow(dat)){
    if(i==1){next}
    if(dat$ID[i]!=dat$ID[(i-1)]){next}
    expdiff=EXPECT_INC[i]-EXPECT_INC[(i-1)]
    unexpdiff=RESID_INC[i]-RESID_INC[(i-1)]
    dPOS_EXPINC[i]=replace(expdiff,expdiff<0,0)
    dPOS_UNEXPINC[i]=replace(unexpdiff,unexpdiff<0,0)
    }

    POS_EXPINC=replace(EXPECT_INC,EXPECT_INC<0,0)
    POS_UNEXPINC=replace(RESID_INC,RESID_INC<0,0)
    NEG_EXPINC=replace(EXPECT_INC,EXPECT_INC>0,0)
    NEG_UNEXPINC=replace(RESID_INC,RESID_INC>0,0)
    dCONS = diff(pdat$CONS)

    return(
        data.frame(
            "POS_EXPINC" = POS_EXPINC,
            "POS_UNEXPINC" = POS_UNEXPINC,
            "NEG_EXPINC" = NEG_EXPINC,
            "NEG_UNEXPINC" = NEG_UNEXPINC,
            "dCONS" = dCONS
        )
    )
}