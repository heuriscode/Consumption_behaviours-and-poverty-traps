get_linear_low = function(){


    eq_exp_inc=INC~lag(INC,1:2)+lag(CONS,1:2)+lag(PARISH_CONS,1:2)

    #use the mg, NOT the ccemg model as common correlated effects are unknown to households before occurence
    predinc_reg=pmg(eq_exp_inc,data=pdat,model="mg")
    #multiple r-squared = 0.46

    ### lagged consumption (both), lagged ratio of disc to total cons (both) and lagged parish consumption (second lag) are all insignificant but evidence that they improve the model
    # joint f-test:
    #   with no lagged PARISH_CON:  F-stat ~ 1.3
    #   OR no DISC_TO_CONS_RAT:  F-stat ~ 1.3
    # so can reject restrictions individually but not a strong rejection
    # 28% of households involved a strong rejection of these restrictions (<5% p-value)
    # so good evidence that, for some households, there is a stronger predictability for household income.

    #F-test
    #Fstat = getFSTATforPGM(predinc_reg_RESTRICT,predinc_reg,pdat)


    #DW-test - provides vector by household, take mean to get mean DW test result
    DWstat = round(pbnftest(predinc_reg)$statistic,2)
    BNF=round(pbnftest(predinc_reg,test="bnf")$statistic,2)
    LBI=round(pbnftest(predinc_reg,test="lbi")$statistic,2)


    #generate results matrix
    len=length(predinc_reg$coef)
    predinc_mat=matrix(NA,nrow=(8+len),ncol=3)
    predinc_mat[1:len,]=round(summary(predinc_reg)$Coef[,-3],3)

    #manually enter values for R2, #hh, #time, #total, and DWtest because I'm feeling brain-lazy for thinking how to code this.
    predinc_mat[(len+2):(len+8),1]=c(0.54,423,22,9306,DWstat,BNF,LBI)


    ### CHECK AND REDO FOR THE FINAL REGRESSSION ###
    rownames(predinc_mat)=c("Intercept","L1(Income)",
                            "L1(Consumption)",
                            "L1 Own Discretionary/Total Consumption Ratio",
                            "L1(Parish mean income)",
                            "L1(Parish mean consumption)",
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