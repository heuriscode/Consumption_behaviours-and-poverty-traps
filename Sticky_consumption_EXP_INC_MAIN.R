#script to analyse diary income, expenditure and savings

#Reference:
# TBD

#This script estimates a model of asymmetrically sticky consumption in which positive income shocks
#generate slower returns to the normal income-consumption equilibrium than do negative income shocks

#The model is estimated under a difference equation framework using a wide panel dataset of 423 households
#  observed across 24 weeks each (observations are weekly).

rm(list=ls())

#set working directory
library(here)
here()
setwd(here())

#load data
load(file="data\\data.Rda")
load(file="data\\bg_data.Rda")

#load libraries
library(plm)         # mean group estimators for panel data
library(ggplot2)     # For simulation plots

#load scripts and programs
source(here("utilities","getDWforPGM.R"))  #manual DW test for MG estimators
source(here("utilities","getFSTATforPGM.R"))  #manual F-test for regression restrictions for MG estimators
source(here("utilities","getplmfitted.R"))  #manual predicted and residuals series calculator for MG estimators as 'predict()' doesn't work for those.
source(here("utilities","get_outmat_ROT_08_12_20.R"))
source(here("utilities","get_outmat_HABIT_08_12_20.R"))
source(here("utilities","get_outmat_JONES_08_12_20.R"))
source(here("utilities","expected_income_linear_middle.R"))

############################################
#NOTES :
############################################
# clustering is not relevant for the MG family of estimators: heteroskedasticity in response is already accounted for
#     directly in the estimation process itself (individual-level estimates that are averaged along with their respective
#     standard errors)

############################################
#VAR NAMES IN DIARY DATA:
#"SURVEY_ID"                   "WEEK_COUNT"
#"PICKER_Y1"                   "DATE_FROM"
#"DATE_TO"                     "WEEK"
#"MONEY_EARNT_PICKING"         "MONEY_EARNT_COFF_SALES"
#"MONEY_EARNT_PARCHMENT"       "MONEY_EARNT_OTHER_WORK"
#"MONEY_EARNT_GOVT_NGO"        "MONEY_EARNT_PARTNER"
#"MONEY_EARNT_FRIEND"          "MONEY_EARNT_SAVINGS"
#"MONEY_EARNT_LOANS"           "GAMBLING"
#"ASK_MONEY"                   "MONEY_FROM_ASK"
#"MONEY_WENT_PARTNER"          "AMOUNT_WENT_PARTNER"
#"MONEY_WENT_EXPENSES"         "MONEY_WENT_SAVINGS"
#"MONEY_WENT_STOLEN"           "MONEY_WENT_FRIENDS"
#"MONEY_WENT_LOAN"             "EXPENSES_FOOD"
#"EXPENSES_FARM_TOOLS"         "EXPENSES_FARM_INPUTS"
#"EXPENSES_HOME_CONSUMABLES"   "EXPENSES_EDUCATION"
#"EXPENSES_GAMBLING"           "EXPENSES_ALCOHOL_CIGARETTES"
#"EXPENSES_HOUSELOAN_RENT"     "EXPENSES_OTHER_LOAN"
#"EXPENSES_TRANSPORT"          "EXPENSES_OTHER"

#first remove households which have less than the full 6 rounds of data = 24 weeks
#
IDvec1=levels(as.factor(data$SURVEY_ID))
nweeks=c()
NWEEKS=c()
PICKER=c()
for(i in 1:length(IDvec1)){
  subdat=data[data$SURVEY_ID==IDvec1[i],]
  nweeks[i]=nrow(subdat)
  NWEEKS=c(NWEEKS,rep(nweeks[i],nrow(subdat)))
  PICKER[i]=data$PICKER_Y1[data$SURVEY_ID==IDvec1[i]][1]
  if(!exists("data1")){
    if(nweeks[i]==24){
      data1=subdat
    }
  } else {
    if(nweeks[i]==24){
      data1=data.frame(rbind(data1,subdat))
    }
  }
}
sum(nweeks!=24) #7 households in this data set (<1.5%)

#check:
nweeks=c()
IDvec2=levels(droplevels(as.factor(data1$SURVEY_ID)))
for(i in 1:length(IDvec2)){
  nweeks[i]=nrow(data1[data1$SURVEY_ID==IDvec2[i],])
}
sum(nweeks!=24) #must be zero
#if zero:
data=data1

###############################################################################
# MATCH DATA IN BACKGROUND AND DIARY SURVEYS AND REMOVE THOSE NOT INCLUDED IN BOTH
###############################################################################
IDdiary=levels(droplevels(as.factor(data$SURVEY_ID)))     #442 - drop levels from previous in case has been set
IDbg=levels(as.factor(bg_data$ID))                        #467

bg_data1=bg_data
matches=sum(IDbg %in% IDdiary)   #442 matches
rowseq=seq(1,nrow(bg_data))
negvec=c()
ind=1

for(i in 1:nrow(bg_data)){
  if(bg_data$ID[i] %in% IDdiary){next}
  negvec[ind]=-rowseq[i]
  ind=ind+1
}

bg_data=bg_data[negvec,]
#nrows in bgdata1 = 442 - fully matching diary data now.

###################
#clean data:
###################
data$MONEY_EARNT_PICKING[data$MONEY_EARNT_PICKING>100000]=100000     #~$37 aud per day or 30 baskets (1 picker will pick max 7-10 baskets per day), 39 Obs censored<1%
data$MONEY_EARNT_COFF_SALES[data$MONEY_EARNT_COFF_SALES>2000000]=2000000    #~1.3 tonnes of cherries sales per week. 2 obs censored
data$MONEY_EARNT_PARCHMENT[data$MONEY_EARNT_PARCHMENT>2500000]=2500000    #~420kg of parchment per sales event. 2 obs. censored.
data$MONEY_EARNT_FRIEND[data$MONEY_EARNT_FRIEND>300000]=300000    #~$110 AUD. 6 obs. censored.
data$MONEY_WENT_SAVINGS[data$MONEY_WENT_SAVINGS>3000000]=3000000    #~$1100 AUD
data$MONEY_WENT_STOLEN[data$MONEY_WENT_STOLEN>300000]=300000    #~$110 AUD. 2 OBS. CENSORED.
data$MONEY_WENT_FRIENDS[data$MONEY_WENT_FRIENDS>300000]=300000    #~$110 AUD. 5 obs censored
data$MONEY_WENT_LOAN[data$MONEY_WENT_LOAN>600000]=600000    #~$1100 AUD. 3 obs. censored.
data$EXPENSES_FOOD[data$EXPENSES_FOOD>400000]=500000    #~$160 AUD. 3 obs. censored
data$EXPENSES_HOME_CONSUMABLES[data$EXPENSES_HOME_CONSUMABLES>500000]=550000    #~$200 AUD. 4 OBS. censored.
data$EXPENSES_ALCOHOL_CIGARETTES[data$EXPENSES_ALCOHOL_CIGARETTES>250000]=250000    #~$110 AUD. 1 obs. censored.
data$EXPENSES_TRANSPORT[data$EXPENSES_TRANSPORT>350000]=350000 #2 obs censored.
data$EXPENSES_OTHER[data$EXPENSES_OTHER>800000]=800000  #7 obs censored.
data$MONEY_WENT_LOAN[data$MONEY_WENT_LOAN>800000]=800000  #2 obs censored.
data$MONEY_EARNT_PARCHMENT[is.na(data$MONEY_EARNT_PARCHMENT)]=0  #replace 2 NA observations with zero

CONSUMPTION = data$MONEY_WENT_FRIENDS +
  data$MONEY_WENT_PARTNER +
  data$EXPENSES_FOOD +
  data$EXPENSES_FARM_TOOLS +
  data$EXPENSES_FARM_INPUTS +
  data$EXPENSES_EDUCATION +
  data$EXPENSES_HOME_CONSUMABLES +
  data$EXPENSES_GAMBLING +
  data$EXPENSES_ALCOHOL_CIGARETTES +
  data$EXPENSES_TRANSPORT +
  data$EXPENSES_OTHER

##############################################
#check for too many zeros in consumption data:
##############################################
zeros=c()
for(i in 1:length(IDvec2)){
  zeros[i]=sum(CONSUMPTION[data$SURVEY_ID==IDvec2[i]]==0)
}

#get IDs from the 2 households with 4 weeks or more missing income and consumption data:
missingobs=IDvec2[zeros>=4]
#remove from bg_data
bg_data=bg_data[-which(bg_data$ID %in% missingobs),]
data=data[-which(data$SURVEY_ID %in% missingobs),]
bg_data$VILLAGE=as.factor(droplevels(bg_data$VILLAGE))       #RESET VILLAGE LEVELS
bg_data$PARISH=as.factor(droplevels(bg_data$PARISH))       #RESET VILLAGE LEVELS
bg_data$SUBCOUNTY=as.factor(droplevels(bg_data$SUBCOUNTY))       #RESET  LEVELS

## Also remove ID 8002 and 3412 as having no variation in parish level discretionary consumption
data = data[-which(data$SURVEY_ID==8002 | data$SURVEY_ID==3412),]
bg_data = bg_data[-which(bg_data$ID==8002 | bg_data$ID==3412),]

###########################################################################
#  CALCULATE VILLAGE-LEVEL MEASURE OF INCOME FOR 'KEEPING UP' HYPOTHESIS
###########################################################################

#VILL_INC_AVG will vary by week and ID
#calculate as week-by-week average of village income not including the reference household

#FIRST CHECK HOUSEHOLD COUNTS OF VILLAGE X WEEK.
vils=data.frame("VIL_NAME"=levels(bg_data$VILLAGE),"HH_COUNT_VIL"=rep(NA,length(levels(bg_data$VILLAGE))))
parish=data.frame("PARISH_NAME"=levels(bg_data$PARISH),"HH_COUNT_PARISH"=rep(NA,length(levels(bg_data$PARISH))))
subcounty=data.frame("SUBCOUNTY_NAME"=levels(bg_data$SUBCOUNTY),"HH_COUNT_SUBCOUNTY"=rep(NA,length(levels(bg_data$SUBCOUNTY))))

for(vvv in 1:nrow(vils)){
  ids_in_vils=bg_data$ID[bg_data$VILLAGE==vils[vvv,1]]
  vils[vvv,2]=length(ids_in_vils)
}

for(ppp in 1:nrow(parish)){
  ids_in_parish=bg_data$ID[bg_data$PARISH==parish[ppp,1]]
  parish[ppp,2]=length(ids_in_parish)
}

for(sss in 1:nrow(subcounty)){
  ids_in_subcounty=bg_data$ID[bg_data$SUBCOUNTY==subcounty[sss,1]]
  subcounty[sss,2]=length(ids_in_subcounty)
}

#38 villages have 2 or less households...
#only 12 parishes have 2 or less households - choose to use parish as the aggregation zone.
#remove these from the dataset to allow calculation of average consumption (net of referent HH)
parish_too_small=parish[parish[,2]<=2,1]
IDsinsmall=bg_data$ID[which(bg_data$PARISH %in% parish_too_small)]
bg_data=bg_data[-which(bg_data$ID %in% IDsinsmall),]
data=data[-which(data$SURVEY_ID %in% IDsinsmall),]

bg_data$VILLAGE=as.factor(droplevels(bg_data$VILLAGE))       #RESET VILLAGE LEVELS
bg_data$PARISH=as.factor(droplevels(bg_data$PARISH))       #RESET VILLAGE LEVELS
bg_data$SUBCOUNTY=as.factor(droplevels(bg_data$SUBCOUNTY))       #RESET  LEVELS

#421 households remaining at this point

##################################
#create main analysis variables:
##################################
INCOME = data$MONEY_EARNT_PICKING + data$MONEY_EARNT_COFF_SALES + data$MONEY_EARNT_PARCHMENT + data$MONEY_EARNT_OTHER_WORK +
  data$MONEY_EARNT_GOVT_NGO + replace(data$GAMBLING,is.na(data$GAMBLING),0)

#All consumption
CONSUMPTION = data$EXPENSES_FOOD +
  data$EXPENSES_FARM_TOOLS +
  data$EXPENSES_FARM_INPUTS +
  data$EXPENSES_EDUCATION +
  data$EXPENSES_HOME_CONSUMABLES +
  data$MONEY_WENT_FRIENDS +
  data$EXPENSES_GAMBLING +
  data$EXPENSES_ALCOHOL_CIGARETTES +
  data$EXPENSES_TRANSPORT +
  data$EXPENSES_OTHER

#Discretionary consumption
DISC_CONS = data$MONEY_WENT_FRIENDS +
  data$EXPENSES_ALCOHOL_CIGARETTES +
  data$EXPENSES_TRANSPORT +
  data$EXPENSES_GAMBLING  +
  data$EXPENSES_OTHER

#'flexible' consumption (discretionary + other consumption that can be delayed)
FLEX_CONS = DISC_CONS +
            data$EXPENSES_FARM_INPUTS +
            data$EXPENSES_HOME_CONSUMABLES

###############################################################################
#  Use natural logarithm with a small addition (~0.35 AUD) for zero obs:
#       zero obs account for very small proportion of all obs ~1.8% of consumption and income.
#       they account for more of nondurable (~7%) and durable (~20%) consumption though as expected

#  Check with natural log and run some correlations and test regressions.
###############################################################################
NET_SAVINGS=data$MONEY_WENT_SAVINGS - data$MONEY_EARNT_SAVINGS
SAV_RATE=NET_SAVINGS/INCOME
LN_INCOME=log((INCOME+1000)/1000)
LN_CONSUMPTION=log((CONSUMPTION+1000)/1000)
LN_DISC_CONS=log((DISC_CONS+1000)/1000)
LN_FLEX_CONS=log((FLEX_CONS+1000)/1000)

#use inverse hyperbolic sin to allow non-negative values and approximate a log.
invhypsin=function(x){
  out=log(x+(x^2+1)^(1/2))
  return(out)}

INC_IHS = invhypsin(INCOME/1000)
CONS_IHS = invhypsin(CONSUMPTION/1000)
DISC_IHS = invhypsin(DISC_CONS/1000)
FLEX_IHS = invhypsin(FLEX_CONS/1000)

cor(LN_INCOME,INC_IHS)  #0.998
cor(LN_CONSUMPTION,CONS_IHS)  #0.998
cor(LN_DISC_CONS,DISC_IHS)  #0.998
cor(LN_FLEX_CONS,FLEX_IHS)  #0.998

#set main vars to the IHS version for 2022 review version
INCOME = INC_IHS
CONSUMPTION = CONS_IHS
DISC_CONS = DISC_IHS
FLEX_CONS = FLEX_IHS
DISC_TO_TOT_CONS_RAT = DISC_CONS/CONSUMPTION
FLEX_TO_TOT_CONS_RAT = FLEX_CONS/CONSUMPTION
DISC_TO_TOT_CONS_RAT = replace(DISC_TO_TOT_CONS_RAT,CONSUMPTION==0,1)
FLEX_TO_TOT_CONS_RAT = replace(FLEX_TO_TOT_CONS_RAT,CONSUMPTION==0,1)

#CALCULATE PARISH-LEVEL AVERAGES AND VARIANCE
PARISH=PARISH_CONS=PARISH_INC=VAR_PARISH_CONS=VAR_PARISH_INC=DIFF_PARISH_CONS=DIFF_PARISH_INC=c()
PARISH_DISC=DIFF_PARISH_DISC=PARISH_FLEX=DIFF_PARISH_FLEX = c()

for(i in 1:nrow(data)){
  #First get parish vector to add to data allowing clustered standard errors
  PARISH[i] = bg_data$PARISH[bg_data$ID==data$SURVEY_ID[i]]

  #get other IDs from Parish
  parish=bg_data$PARISH[bg_data$ID==data$SURVEY_ID[i]]
  parish_vec=bg_data$ID[which(bg_data$PARISH %in% parish)]
  parish_vec=parish_vec[parish_vec != data$SURVEY_ID[i]]
  #get week number
  weeknum=data$WEEK[i]

  #get CONSUMPTION for village:
  CONS_PARISH=CONSUMPTION[which(data$SURVEY_ID %in% parish_vec & data$WEEK %in% weeknum)]
  INC_PARISH=INCOME[which(data$SURVEY_ID %in% parish_vec & data$WEEK %in% weeknum)]
  DISC_PARISH=DISC_CONS[which(data$SURVEY_ID %in% parish_vec & data$WEEK %in% weeknum)]
  FLEX_PARISH=FLEX_CONS[which(data$SURVEY_ID %in% parish_vec & data$WEEK %in% weeknum)]
  #enter average value
  PARISH_CONS[i]=mean(CONS_PARISH)
  PARISH_INC[i]=mean(INC_PARISH)
  VAR_PARISH_CONS[i]=var(CONS_PARISH)
  VAR_PARISH_INC[i]=var(INC_PARISH)
  PARISH_DISC[i]=mean(DISC_PARISH)
  PARISH_FLEX[i]=mean(FLEX_PARISH)

  DIFF_PARISH_DISC[i]=PARISH_DISC[i]-DISC_CONS[i]
  DIFF_PARISH_FLEX[i]=PARISH_FLEX[i]-FLEX_CONS[i]
  DIFF_PARISH_CONS[i]=PARISH_CONS[i]-CONSUMPTION[i]
  DIFF_PARISH_INC[i]=PARISH_INC[i]-INCOME[i]
  }


IDvec=levels(as.factor(data$SURVEY_ID))

#CHECK TOTAL CONSUMPTION AND INCOME ACROSS PERIOD:
TOTCONS=TOTDISC_CONS=TOTFLEX_CONS=TOTINC=TOTSAV=c()
for(i in 1:length(IDvec)){
  TOTCONS[i]=sum(CONSUMPTION[data$SURVEY_ID==IDvec[i]],na.rm=TRUE)
  TOTINC[i]=sum(INCOME[data$SURVEY_ID==IDvec[i]],na.rm=TRUE)
  TOTDISC_CONS[i]=sum(DISC_CONS[data$SURVEY_ID==IDvec[i]],na.rm=TRUE)
  TOTFLEX_CONS[i]=sum(FLEX_CONS[data$SURVEY_ID==IDvec[i]],na.rm=TRUE)
  TOTSAV[i]=sum(NET_SAVINGS[data$SURVEY_ID==IDvec[i]],na.rm=TRUE)
  }


#get a summary of the data first:
data_summary_mat=rbind(c(quantile(exp(INCOME),probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE),
                       mean(exp(INCOME)),
                       sqrt(var(exp(INCOME)))),
                       c(quantile(exp(CONSUMPTION),probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE),
                       mean(exp(CONSUMPTION)),
                       sqrt(var(exp(CONSUMPTION)))),
                       c(quantile(exp(DISC_CONS),probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE),
                       mean(exp(DISC_CONS)),
                       sqrt(var(exp(DISC_CONS)))),
                       c(quantile(exp(FLEX_CONS),probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE),
                       mean(exp(FLEX_CONS)),
                       sqrt(var(exp(FLEX_CONS))))
                       )
#data_summary_mat[,1]=0 #replace 100 shillings for logarithm with 0
colnames(data_summary_mat)=c("Minimum","25th percentile","Median","75th percentile","Maximum","Mean","Std. Deviation")
rownames(data_summary_mat)=c("INCOME","CONSUMPTION TOTAL","DISCRETIONARY CONSUMPTION","FLEXIBLE CONSUMPTION")
write.table(data_summary_mat,"results\\data summary.csv",sep=",")

WEEKS=max(data$WEEK)


################################
#CREATE PANEL DATA FRAME
####################################

dat=data.frame(data$SURVEY_ID,PARISH,data$WEEK,
               INCOME,CONSUMPTION,
               DISC_CONS,DISC_TO_TOT_CONS_RAT,
               FLEX_CONS,FLEX_TO_TOT_CONS_RAT,
               PARISH_CONS,PARISH_INC,PARISH_DISC,PARISH_FLEX,
               DIFF_PARISH_CONS,DIFF_PARISH_INC,DIFF_PARISH_DISC,DIFF_PARISH_FLEX,
               VAR_PARISH_CONS,VAR_PARISH_INC)
names(dat)=c("ID","PARISH","WEEK",
             "INC","CONS",
             "DISC","DISC_TO_TOT_CONS_RAT",
             "FLEX","FLEX_TO_TOT_CONS_RAT",
             "PARISH_CONS","PARISH_INC","PARISH_DISC","PARISH_FLEX",
             "DIFF_PARISH_CONS","DIFF_PARISH_INC","DIFF_PARISH_DISC","DIFF_PARISH_FLEX",
             "VAR_PARISH_CONS","VAR_PARISH_INC")

pdat=pdata.frame(dat,index=c("ID","WEEK"))

INCPOS=replace(pdat$INC,pdat$INC<0,0)
CONSPOS=replace(pdat$CONS,pdat$CONS<0,0)
DISCPOS=replace(pdat$DISC,pdat$DISC<0,0)
FLEXPOS=replace(pdat$FLEX,pdat$FLEX<0,0)
dINCPOS=replace(diff(pdat$INC),diff(pdat$INC)<0,0)
dCONSPOS=replace(diff(pdat$CONS),diff(pdat$CONS)<0,0)
dDISCPOS=replace(diff(pdat$DISC),diff(pdat$DISC)<0,0)
dFLEXPOS=replace(diff(pdat$FLEX),diff(pdat$FLEX)<0,0)
PARISH_CONS_POS=replace(diff(pdat$PARISH_CONS),diff(pdat$PARISH_CONS)<0,0)
PARISH_DISC_POS=replace(diff(pdat$PARISH_DISC),diff(pdat$PARISH_DISC)<0,0)
PARISH_FLEX_POS=replace(diff(pdat$PARISH_FLEX),diff(pdat$PARISH_FLEX)<0,0)
PARISH_INC_POS=replace(diff(pdat$PARISH_INC),diff(pdat$PARISH_INC)<0,0)
DIFF_PARISH_CONS_POS=replace(pdat$DIFF_PARISH_CONS,pdat$DIFF_PARISH_CONS<0,0)
DIFF_PARISH_DISC_POS=replace(diff(pdat$DIFF_PARISH_DISC),diff(pdat$DIFF_PARISH_DISC)<0,0)
DIFF_PARISH_FLEX_POS=replace(diff(pdat$DIFF_PARISH_FLEX),diff(pdat$DIFF_PARISH_FLEX)<0,0)

dat=data.frame(dat,INCPOS,CONSPOS,DISCPOS,FLEXPOS,dINCPOS,dCONSPOS,dDISCPOS,dFLEXPOS,
               PARISH_CONS_POS,PARISH_DISC_POS,PARISH_FLEX_POS,PARISH_INC_POS,DIFF_PARISH_CONS_POS,DIFF_PARISH_DISC_POS,DIFF_PARISH_FLEX_POS)
pdat=pdata.frame(dat,index=c("ID","WEEK"))


#############################################################
#  GET EXPECTED INCOME BASED ON EXOGENOUS VARIABLES
#############################################################

#diff(INC) lags are known to the household and have been experienced so are exogenous and known
#mean parish income (not including the household) is exogenous and is relatively easily observed by most households
#the DISC_TO_TOT_CONS_RAT term is a ratio of discretionary consumption to total consumption (lagged) - an indicator of their previous consumption patterns/preferences

### Note:
#  predict function for MG type models uses *individual* level models and associated predictions
#  so heterogeneity in expectations is also accounted for using these models
#  it is better to allow for some DF losses as this allows for some HH to have great 'rationality' over others (e.g. include some variables that are, on average, insignificant)
#  this also implies that the use of a trend term is only appropriate if income is smooth - otherwise it will over-smooth predicted income. Thus, leave out as will be driven by outliers
#  such as random higher consumption at the start, middle, or end of the sample period would generate decreasing, inverted-U, or increasing expected income predictions.

#run three different expected income functions:
#  1. A high-predictive power one (gams function)
#  2. A target predictive one (using linear estimator with all components)
#  3. A low-predictive power one (linear estimator with fewer components)

### Note2: These are implemented as different main scripts - see main folder and run these separately.

res = get_linear_middle()

## Create different pdata frames for each of the different expected income models
pdat=pdata.frame(data.frame(
    dat,"EXPECTED_INC"=res$EXPECT_INC,
    "UNEXPECTED_INC"=res$RESID_INC,
    "POS_EXPINC"=res$NEG_EXPINC,
    "POS_UNEXPINC"=res$NEG_UNEXPINC,
    "POS_EXPINC"=res$POS_EXPINC,
    "POS_UNEXPINC"=res$POS_UNEXPINC,
    "dPOS_EXPINC"=res$dPOS_EXPINC,
    "dPOS_UNEXPINC"=res$dPOS_UNEXPINC
    ),
    index=c("ID","WEEK")
)

###################################
#  HISTOGRAMS FOR CONSUMPTION/INCOME
###################################

HH_TOT_CONS=c()
HH_TOT_INC=c()
for(i in 1:length(IDvec)){
  subdat=pdat[pdat$ID==IDvec[i],]
  HH_TOT_CONS[i]=sum(subdat$CONS)
  HH_TOT_INC[i]=sum(subdat$INC)
}

TOT_CONS=c()
TOT_INC=c()
for(www in 1:24){
  subdat=pdat[pdat$WEEK==www,]
  TOT_CONS[www]=sum(subdat$CONS)
  TOT_INC[www]=sum(subdat$INC)
}

incrat_hh=HH_TOT_CONS/HH_TOT_INC
incrat_all=TOT_CONS/TOT_INC
par(mfrow=c(1,2))
hist(incrat_hh,main="Household-level total consumption/income",xlab="Consumption/Income ratio")
hist(incrat_all,main="Sample-level weekly consumption/income",xlab="Consumption/Income ratio",breaks=seq(0.85,1.15,0.05))


###################################
#  PERSISTENCE IN INCOME
###################################

reg_persistenceINC=pmg(diff(INC)~lag(diff(INC),1:3)+lead(diff(INC),1),data=pdat,model="mg",trend=TRUE)
reg_persistenceUNEXPINC=pmg(diff(UNEXPECTED_INC)~lag(diff(UNEXPECTED_INC),1:3)+lead(diff(UNEXPECTED_INC),1),data=pdat,model="mg",trend=TRUE)
reg_persistenceEXPINC=pmg(diff(EXPECTED_INC)~lag(diff(EXPECTED_INC),1:3)+lead(diff(EXPECTED_INC),1),data=pdat,model="mg",trend=TRUE)

persist_mat=matrix(NA,nrow=(5+1+7),ncol=3)
rownames(persist_mat)=c("Intercept","L1(dep. var.)","L2(dep. var.)","L3(dep. var.)","P1(dep. var.)","","R-squared","# households","Time periods used","Total # observations",
                        "DW statistic","Barghava et al Durbin Watson Statistic","Baltagi-Wu LBI statistic")
colnames(persist_mat)=c("Income","Expected Income","Unexpected Income")

DWstat_I=round(pbnftest(reg_persistenceINC)$statistic,2)
BNF_I=round(pbnftest(reg_persistenceINC,test="bnf")$statistic,2)
LBI_I=round(pbnftest(reg_persistenceINC,test="lbi")$statistic,2)

DWstat_E = round(pbnftest(reg_persistenceEXPINC)$statistic,2)
BNF_E=round(pbnftest(reg_persistenceEXPINC,test="bnf")$statistic,2)
LBI_E=round(pbnftest(reg_persistenceEXPINC,test="lbi")$statistic,2)

DWstat_U = round(pbnftest(reg_persistenceUNEXPINC)$statistic,2)
BNF_U=round(pbnftest(reg_persistenceUNEXPINC,test="bnf")$statistic,2)
LBI_U=round(pbnftest(reg_persistenceUNEXPINC,test="lbi")$statistic,2)


modlist=list(reg_persistenceINC,reg_persistenceEXPINC,reg_persistenceUNEXPINC)

# Create the output matrix including model test statistics, r-squared, etc.
modelstatmat=cbind(
  c(
      summary(modlist[[1]])$rsqr,                                         #rsqr
      (dim(modlist[[1]]$indcoef)[2]),                                     #num households
      length(modlist[[1]]$fitted.values)/(dim(modlist[[1]]$indcoef)[2]),  #time periods used
      length(modlist[[1]]$fitted.values),                                 #total observations used
      DWstat_I,                                                           #Durbin Watson statistic
      BNF_I,                                                              #BNF statistic
      LBI_I                                                               #LBI statistic
    ),                                                             
  c(
      summary(modlist[[2]])$rsqr,                                         #rsqr
      (dim(modlist[[2]]$indcoef)[2]),                                     #num households
      length(modlist[[2]]$fitted.values)/(dim(modlist[[2]]$indcoef)[2]),  #time periods used
      length(modlist[[2]]$fitted.values),                                 #total observations used
      DWstat_E,                                                           #Durbin Watson statistic
      BNF_E,                                                              #BNF statistic
      LBI_E                                                               #LBI statistic
    ), 
  c(
      summary(modlist[[3]])$rsqr,                                         #rsqr
      (dim(modlist[[3]]$indcoef)[2]),                                     #num households
      length(modlist[[3]]$fitted.values)/(dim(modlist[[3]]$indcoef)[2]),  #time periods used
      length(modlist[[3]]$fitted.values),                                 #total observations used
      DWstat_U,                                                           #Durbin Watson statistic
      BNF_U,                                                              #BNF statistic
      LBI_U                                                               #LBI statistic
    ) 
)

#P-value stars and coefficient formatting
for(rrr in 1:5){
  for(ccc in 1:length(modlist)){
    mod=summary(modlist[[ccc]])$Coef
    stars_=ifelse(mod[3,4]>0.1,"",ifelse(mod[3,4]>0.05,"*",ifelse(mod[3,4]>0.1,"**","***")))
    persist_mat[rrr,ccc]=paste(round(mod[rrr,1],3),stars_," (",round(mod[rrr,2],3),")",sep="")
  }
}

#Final formatting/creation of output table for income shock persistence
persist_mat[7:13,1]=modelstatmat[,1]
persist_mat[7:13,2]=modelstatmat[,2]
persist_mat[7:13,3]=modelstatmat[,3]

#Write to results folder
write.table(persist_mat,"results\\income persistence results_MAIN.csv",sep=",")

################################
#CAUSALITY CHECK:
################################
# This section estimates the reverse causality check
#    that seeks to ensure that consumption does not granger
#    cause income. 

reg_causalityUC=pmg(diff(UNEXPECTED_INC)~lag(diff(CONS),1)+
                      lag(diff(EXPECTED_INC),1)+
                      lag(diff(UNEXPECTED_INC),1)+
                      lag(diff(PARISH_INC),1),
                    data=pdat,model="cmg")

#R-squared = 0.53
#n=423
#N=8460
#T=20
DWstat = round(pbnftest(reg_causalityUC)$statistic,2)
BNF=round(pbnftest(reg_causalityUC,test="bnf")$statistic,2)
LBI=round(pbnftest(reg_causalityUC,test="lbi")$statistic,2)
coefmatUC=summary(reg_causalityUC)$Coef
caus_mat=matrix(NA,nrow=(5+8),ncol=2)

#coefficient of interest is third in each regression (the lag of the cons/unexpected income)

#UC
for(i in 1:5){
  stars_=ifelse(coefmatUC[i,4]>0.1,"",ifelse(coefmatUC[i,4]>0.05,"*",ifelse(coefmatUC[i,4]>0.1,"**","***")))
  est = paste(round(coefmatUC[i,1],2),stars_,sep="")
  stderr = paste("(",round(coefmatUC[i,2],2),")",sep="")
  caus_mat[i,]=c(est,stderr)
}

caus_mat[-1:-6,1]=c(
      summary(reg_causalityUC)$rsqr,                                         #rsqr
      (dim(reg_causalityUC$indcoef)[2]),                                     #num households
      length(reg_causalityUC$fitted.values)/(dim(reg_causalityUC$indcoef)[2]),  #time periods used
      length(reg_causalityUC$fitted.values),
      DWstat,
      BNF,
      LBI)

rownames(caus_mat)=c("Intercept","Lag(diff(Consumption))","Lag(diff(Expected Income))","Lag(diff(Unexpected Income))","Lag(diff(Mean Parish Income))",
                     "","R squared","# households","Time periods used","Total # observations",
                     "DW-statistic","Baltagi-Wu LBI statistic","Barghava et al Durbin Watson Statistic")
colnames(caus_mat)=c("Estimate","Standard Error")

write.table(caus_mat,"results\\causality results_MAIN.csv",sep=",")

##################################################
# MAIN regressions TOTAL CONSUMPTION
##################################################

#estimate models
source(here("utilities","estimateModels.R"))
estimated_models = estimateModels(reg_df=pdat)

rotlist = estimated_models$rotList
habitlist = estimated_models$habitList
joneslist = estimated_models$jonesList


###############################
#   generate main results
###############################

#16 models - 4 each in symmetric, 3 each in asymmetric (some symmetric models are shared)
#so 7 model results in each table (columns)

#each table needs r-squared and n, t and NT
#rot has max 4 parameters, the others have max 2

#ROT
source("utilities\\get_outmat_ROT_08_12_20.R")
outmat_rot=get_outmat_ROT(rotlist,intercept=FALSE)

#HABITS
source("utilities\\get_outmat_HABIT_08_12_20.R")
outmat_habit=get_outmat_HABIT(habitlist,intercept=FALSE)

#JONES
source("utilities\\get_outmat_JONES_08_12_20.R")
outmat_jones=get_outmat_JONES(joneslist,intercept=FALSE)

#write tables
write.table(outmat_rot,"results\\RoT results full sample_MAIN.csv",sep=",",row.names=TRUE)
write.table(outmat_habit,"results\\Habits results full sample_MAIN.csv",sep=",",row.names=TRUE)
write.table(outmat_jones,"results\\Jones results full sample_MAIN.csv",sep=",",row.names=TRUE)

###############################
#   Simulations
###############################

#In this section, we use the coefficients in the estimation to demonstrate dynamics in income, consumption and asset accumulation. 

source("utilities\\runSimulations.R")
simulation_list = runSimulations(bg_data, data, rotlist, joneslist, RESID_INC)

# END of SCRIPT



