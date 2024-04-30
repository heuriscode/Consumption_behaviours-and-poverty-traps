## core estimation function

estimateModels = function(pdat){
  
    reg_df = pdat

    ## estimate models
    rot_asym_CONS=pmg(eq_rot_asym_CONS,model="cmg",data=reg_df)
    rot_habit_jones_asym_CONS=pmg(eq_rot_habit_jones_asym_CONS,model="cmg",data=reg_df)
    rot_asym_DISC_CONS=pmg(eq_rot_asym_DISC_CONS,model="cmg",data=reg_df)
    rot_habit_jones_asym_DISC_CONS=pmg(eq_rot_habit_jones_asym_DISC_CONS,model="cmg",data=reg_df)

    habit_asym_CONS=pmg(eq_habit_asym_CONS,model="cmg",data=reg_df)
    habit_rot_jones_asym_CONS=pmg(eq_habit_rot_jones_asym_CONS,model="cmg",data=reg_df)
    habit_asym_DISC_CONS=pmg(eq_habit_asym_DISC_CONS,model="cmg",data=reg_df)
    habit_rot_jones_asym_DISC_CONS=pmg(eq_habit_rot_jones_asym_DISC_CONS,model="cmg",data=reg_df)

    #note these two hh both have all zero DIFF_PARISH_CONS_POS so generate NA for that - remove those two hh
    jones_asym_CONS=pmg(eq_jones_asym_CONS,model="cmg",data=reg_df,subset=(reg_df$ID!=8002 & reg_df$ID!=3412))
    jones_rot_habit_asym_CONS=pmg(eq_jones_rot_habit_asym_CONS,model="cmg",data=reg_df,subset=(reg_df$ID!=8002 & reg_df$ID!=3412))
    
    #note one hh both has all zero DIFF_PARISH_DISC_CONS_POS so generate NA for that - remove 
    jones_asym_DISC_CONS=pmg(eq_jones_asym_DISC_CONS,model="cmg",data=reg_df,subset=(reg_df$ID!=8002))
    jones_rot_habit_asym_DISC_CONS=pmg(eq_jones_rot_habit_asym_DISC_CONS,model="cmg",data=reg_df,subset=(reg_df$ID!=8002))

    rotlist=list(rot_asym_CONS,rot_habit_jones_asym_CONS,
             rot_asym_DISC_CONS,rot_habit_jones_asym_DISC_CONS)

    habitlist=list(habit_asym_CONS,habit_rot_jones_asym_CONS,
               habit_asym_DISC_CONS,habit_rot_jones_asym_DISC_CONS)

    joneslist=list(jones_asym_CONS,jones_rot_habit_asym_CONS,
               jones_asym_DISC_CONS,jones_rot_habit_asym_DISC_CONS)
    
    return(list(
        'rotlist' = rotlist,
        'habitlist' = habitlist,
        'joneslist' = joneslist
    ))
}