## core estimation function

estimateModels = function(reg_df){

    #ROT:
    #PIH predicts consumption = expected income
    #PIH predicts saving of a large portion of unexpected income (spreading across time)
    #So H0 is that the unexpected income coefficients are = 0.
    #If positive then significant ROT effects.
    eqRotList = list(
        eq_rot_asym_CONS=diff(CONS)~0+diff(EXPECTED_INC)+dPOS_EXPINC+diff(UNEXPECTED_INC)+dPOS_UNEXPINC,
        eq_rot_habit_jones_asym_CONS=diff(CONS)~0+diff(EXPECTED_INC)+dPOS_EXPINC+diff(UNEXPECTED_INC)+dPOS_UNEXPINC+lag(DIFF_PARISH_CONS)+lag(diff(CONS),1),
        eq_rot_asym_DISC_CONS=diff(DISC)~0+diff(EXPECTED_INC)+dPOS_EXPINC+diff(UNEXPECTED_INC)+dPOS_UNEXPINC,
        eq_rot_habit_jones_asym_DISC_CONS=diff(DISC)~0+diff(EXPECTED_INC)+dPOS_EXPINC+diff(UNEXPECTED_INC)+dPOS_UNEXPINC+lag(DIFF_PARISH_FLEX)+lag(diff(DISC),1),
        eq_rot_asym_FLEX_CONS=diff(FLEX)~0+diff(EXPECTED_INC)+dPOS_EXPINC+diff(UNEXPECTED_INC)+dPOS_UNEXPINC,
        eq_rot_habit_jones_asym_FLEX_CONS=diff(FLEX)~0+diff(EXPECTED_INC)+dPOS_EXPINC+diff(UNEXPECTED_INC)+dPOS_UNEXPINC+lag(DIFF_PARISH_FLEX)+lag(diff(FLEX),1)
    )

    #HABITS:
    # lagged consumption changes are sought to be maintained (habits)
    # H0 is for PIH - that lagged consumption changes have no effect on current consumption shocks
    # Rho between -1 and above 0 indicates habit formation.
    eqHabitList = list(
        eq_habit_asym_CONS=diff(CONS)~0+lag(diff(CONS),1)+lag(dCONSPOS,1)+diff(EXPECTED_INC),
        eq_habit_rot_jones_asym_CONS=diff(CONS)~0+lag(diff(CONS),1)+lag(dCONSPOS,1)+lag(DIFF_PARISH_CONS)+diff(EXPECTED_INC)+diff(UNEXPECTED_INC),
        eq_habit_asym_DISC_CONS=diff(DISC)~0+lag(diff(DISC),1)+lag(dDISCPOS,1)+diff(EXPECTED_INC),
        eq_habit_rot_jones_asym_DISC_CONS=diff(DISC)~0+lag(diff(DISC),1)+lag(dDISCPOS,1)+lag(DIFF_PARISH_DISC)+diff(EXPECTED_INC)+diff(UNEXPECTED_INC),
        eq_habit_asym_FLEX_CONS=diff(FLEX)~0+lag(diff(FLEX),1)+lag(dFLEXPOS,1)+diff(EXPECTED_INC),
        eq_habit_rot_jones_asym_FLEX_CONS=diff(FLEX)~0+lag(diff(FLEX),1)+lag(dFLEXPOS,1)+lag(DIFF_PARISH_FLEX)+diff(EXPECTED_INC)+diff(UNEXPECTED_INC)
    )

    #KEEPING UP WITH THE JONESES:
    #Differences between own consumption and parish mean consumption are sought to be reduced
    #H0 is that housholds maximise a purely inward looking utility function that is not dependent on parish level consumption
    #So H0 is that parish level differences from own consumption are not significant
    #positive coefficients indicate a significant Keeping Up effect.
    eqJonesList = list(
        eq_jones_asym_CONS=diff(CONS)~0+lag(DIFF_PARISH_CONS,1)+lag(DIFF_PARISH_CONS_POS,1)+diff(EXPECTED_INC),
        eq_jones_rot_habit_asym_CONS=diff(CONS)~0+lag(DIFF_PARISH_CONS,1)+lag(DIFF_PARISH_CONS_POS,1)+lag(diff(CONS),1)+diff(EXPECTED_INC)+diff(UNEXPECTED_INC),
        eq_jones_asym_DISC_CONS=diff(DISC)~0+lag(DIFF_PARISH_DISC)+lag(DIFF_PARISH_DISC_POS)+diff(EXPECTED_INC),
        eq_jones_rot_habit_asym_DISC_CONS=diff(DISC)~0+lag(DIFF_PARISH_DISC)+lag(DIFF_PARISH_DISC_POS)+lag(diff(DISC),1)+diff(EXPECTED_INC)+diff(UNEXPECTED_INC),
        eq_jones_asym_FLEX_CONS=diff(FLEX)~0+lag(DIFF_PARISH_FLEX)+lag(DIFF_PARISH_FLEX_POS)+diff(EXPECTED_INC),
        eq_jones_rot_habit_asym_FLEX_CONS=diff(FLEX)~0+lag(DIFF_PARISH_FLEX)+lag(DIFF_PARISH_FLEX_POS)+lag(diff(FLEX),1)+diff(EXPECTED_INC)+diff(UNEXPECTED_INC)
    )

    #Initialise list vectors
    rotList = habitList = jonesList = vector(length=length(eqRotList),mode="list")

    #Estimate ROT models from list of equations
    for(mmm in 1:length(eqRotList)){
        rotList[[mmm]]=pmg(eqRotList[[mmm]],model="cmg",data=reg_df)
    }
    
    #Estimate Habit models from list of equations
    for(mmm in 1:length(eqHabitList)){
        habitList[[mmm]]=pmg(eqHabitList[[mmm]],model="cmg",data=reg_df)
    }

    #Estimate Jones models from list of equations
    for(mmm in 1:length(eqJonesList)){
        jonesList[[mmm]]=pmg(eqJonesList[[mmm]],model="cmg",data=reg_df)
    }

    names(rotList)=c(
        "rot_asym_CONS",
        "rot_habit_jones_asym_CONS",
        "rot_asym_DISC_CONS",
        "rot_habit_jones_asym_DISC_CONS",
        "rot_asym_FLEX_CONS",
        "rot_habit_jones_asym_FLEX_CONS"
        )

    names(habitList)=c(
        "habit_asym_CONS",
        "habit_rot_jones_asym_CONS",
        "habit_asym_DISC_CONS",
        "habit_rot_jones_asym_DISC_CONS",
        "habit_asym_FLEX_CONS",
        "habit_rot_jones_asym_FLEX_CONS"
        )

    names(jonesList)=c(
        "jones_asym_CONS",
        "jones_rot_habit_asym_CONS",
        "jones_asym_DISC_CONS",
        "jones_rot_habit_asym_DISC_CONS",
        "jones_asym_FLEX_CONS",
        "jones_rot_habit_asym_FLEX_CONS"
        )
    
    return(list(
        'rotList' = rotList,
        'habitList' = habitList,
        'jonesList' = jonesList
    ))
}