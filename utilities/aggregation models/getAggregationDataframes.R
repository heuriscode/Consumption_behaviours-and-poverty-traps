## returns aggregated dataframes for testing of aggregation concerns

getAggregatedDataframes = function(){
    # define the core dataset to use to check aggregation concerns (this is the regression dataset above)
    agg_dat = pdat #note that this includes variables estimated/calculated using the full data - thus is an optimistic test
    ## NB first three columns are ID, PARISH, WEEK. 

    # use the 'WEEK' and 'SURVEY_ID' variables to aggregate.
    HH_IND = levels(as.factor(data$SURVEY_ID))
    WEEK_IND = levels(as.factor(data$WEEK))

    # loop through each of these with different aggregation levels of 2, 4, and 6 weeks. 
    AGG_IND_VECTOR = c(2,4,6) #resp. 12 obs, 6 obs, and 4 obs per household

    # check all HH have equal weeks for aggregation (remove those that do not)
    hh_check=c()
    for(i in 1:length(HH_IND)){
    hh_check[i] = nrow(pdat[pdat$ID==HH_IND[i],]) #nrow of each hh data subset
    }
    sum(hh_check==24)==length(HH_IND) #must equal 'TRUE'

    # Loop through to aggregate the data for each aggregation series
    for(HH in 1:length(HH_IND)){
    hh_ind = HH_IND[[HH]]
    parish = agg_dat$PARISH[agg_dat$ID==hh_ind][1]

    #set subdat as data for ID==hh_ind
    subdat = pdat[(pdat$ID==hh_ind),-1:-3] #remove ID, PARISH and WEEK
        
    ## 2 week aggregation
    for(WW in 1:(length(WEEK_IND)/2)){
        W_AGG = 2
        nrows = 24/W_AGG

        #set subdat for weeks index
        subdat_ = subdat[(WW-1)*W_AGG+1:WW*W_AGG,] #index the relevant weeks to aggregate
        aggregated_data = apply(subdat_,2,mean) #take average for each column across the relevant weeks
        #create dataframe if first element of loop  
        if(HH==1 && WW==1){
        agg_dat_W2 = aggregated_data #put 
        #else bind dfs together
        } else {
        agg_dat_W2 = rbind(agg_dat_W2,aggregated_data)
        }
    }
    #add back ID, parish, weeks vector
    agg_dat_W2 = data.frame(
        rep(hh_ind,nrows),
        rep(parish,nrows),
        seq(1,nrows),
        agg_dat_W2)
        
    #add names to df
    colnames(agg_dat_W2) = colnames(agg_dat)

    
    ## 4 week aggregation
    for(WW in 1:(length(WEEK_IND)/4)){
        W_AGG = 4
        nrows = 24/W_AGG

        #set subdat for weeks index
        subdat_ = subdat[(WW-1)*W_AGG+1:WW*W_AGG,] #index the relevant weeks to aggregate
        aggregated_data = apply(subdat_,2,mean) #take average for each column across the relevant weeks
        #create dataframe if first element of loop  
        if(HH==1 && WW==1){
        agg_dat_W4 = aggregated_data 
        #else bind dfs together
        } else {
        agg_dat_W4 = rbind(agg_dat_W4,aggregated_data)
        }
    }

    #add back ID, parish, weeks vector
    agg_dat_W4 = data.frame(
        rep(hh_ind,nrows),
        rep(parish,nrows),
        seq(1,nrows),
        agg_dat_W4)
        
    #add names to df
    colnames(agg_dat_W4) = colnames(agg_dat)

    ## 6 week aggregation
    for(WW in 1:(length(WEEK_IND)/6)){
        W_AGG = 6
        nrows = 24/W_AGG

        #set subdat for weeks index
        subdat_ = subdat[(WW-1)*W_AGG+1:WW*W_AGG,] #index the relevant weeks to aggregate
        aggregated_data = apply(subdat_,2,mean) #take average for each column across the relevant weeks
        #create dataframe if first element of loop  
        if(HH==1 && WW==1){
        agg_dat_W6 = aggregated_data
        #else bind dfs together
        } else {
        agg_dat_W6 = rbind(agg_dat_W6,aggregated_data)
        }
    }

    agg_dat_W6 = data.frame(
        rep(hh_ind,nrows),
        rep(parish,nrows),
        seq(1,nrows),
        agg_dat_W6
        )
        
    #add names to df
    colnames(agg_dat_W6) = colnames(agg_dat)

    }



    return(list(
        "two_weeks_aggregation" = agg_dat_W2,
        "four_weeks_aggregation" = agg_dat_W4,
        "six_weeks_aggregation" = agg_dat_W6
    ))
}