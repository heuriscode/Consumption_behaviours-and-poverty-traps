# estimate core models for aggregated dataframes and return list of model results

getAggregatedModels = function() {

    ## dataframes are in the 'aggregated_dfs' list object


    ## estimate models
    rot_asym_CONS=pmg(eq_rot_asym_CONS,model="cmg",data=df)
    rot_habit_jones_asym_CONS=pmg(eq_rot_habit_jones_asym_CONS,model="cmg",data=pdat)
    rot_asym_DISC_CONS=pmg(eq_rot_asym_DISC_CONS,model="cmg",data=pdat)
    rot_habit_jones_asym_DISC_CONS=pmg(eq_rot_habit_jones_asym_DISC_CONS,model="cmg",data=pdat)

    habit_asym_CONS=pmg(eq_habit_asym_CONS,model="cmg",data=pdat)
    habit_rot_jones_asym_CONS=pmg(eq_habit_rot_jones_asym_CONS,model="cmg",data=pdat)
    habit_asym_DISC_CONS=pmg(eq_habit_asym_DISC_CONS,model="cmg",data=pdat)
    habit_rot_jones_asym_DISC_CONS=pmg(eq_habit_rot_jones_asym_DISC_CONS,model="cmg",data=pdat)

    jones_asym_CONS=pmg(eq_jones_asym_CONS,model="cmg",data=pdat,subset=(pdat$ID!=8002 & pdat$ID!=3412))  #note these two hh both have all zero DIFF_PARISH_CONS_POS so generate NA for that - remove those two hh
    jones_rot_habit_asym_CONS=pmg(eq_jones_rot_habit_asym_CONS,model="cmg",data=pdat,subset=(pdat$ID!=8002 & pdat$ID!=3412))   #note these two hh both have all zero DIFF_PARISH_CONS_POS so generate NA for that - remove those two hh
    jones_asym_DISC_CONS=pmg(eq_jones_asym_DISC_CONS,model="cmg",data=pdat)
    jones_rot_habit_asym_DISC_CONS=pmg(eq_jones_rot_habit_asym_DISC_CONS,model="cmg",data=pdat,subset=(pdat$ID!=8002))

}


    
    