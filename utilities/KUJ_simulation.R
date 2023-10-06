# This function generates the simulated model for the Keeping up with the Joneses
# Gamma matrices contain the individual coefficients for KUJ behaviour. 
# The function prints out simulated results for the change in savings under KUJ, and asymmetric KUJ. 

KUJ_simulation = function(expected_income, 
                          actual_income,
                          savings,
                          gamma_mat,
                          gammapos_mat,
                          annual_asset_level,
                          interest_rate,
                          minimum_percapita_income,
                          closing_consumption_level) {
  
  #Retrieve the parameters from the matrix dimensions
  n_households = ncol(expected_income)
  n_weeks = nrow(expected_income)
  n_years = nrow(annual_asset_level)-1 # the matrix is designed to capture closing asset level for periods 0:n_years

  #Initialise the weekly consumption matrix. 
  consumption_mat_KUJ = consumption_mat_KUJ_asymmetric = matrix(NA, nrow = n_weeks, ncol = n_households)
  unexpected_income_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  
  ihs_func = function(x) {
    y = asinh(x/1000)
    return(y)
  }
  
  expected_income = ihs_func(expected_income)
  actual_income = ihs_func(actual_income)
  savings = ihs_func(savings)
  annual_asset_level = ihs_func(annual_asset_level) # But we let households with existing debt to start in debt. 
  minimum_percapita_income = ihs_func(minimum_percapita_income)
  sd_unexpected_inc = sd(expected_income) # As this matrix changes in the loop, pre-define the standard deviation of unexpected income
  
  #Pre-define the matrices to ensure they are consistent with both model runs. 
  savings_KUJ = savings_KUJ_asymmetry = savings
  expected_income_KUJ =  expected_income_KUJ_asymmetry = expected_income
  actual_income_KUJ =  actual_income_KUJ_asymmetry = actual_income
  annual_asset_level_KUJ = annual_asset_level_KUJ_asymmetry = annual_asset_level
  
  # KUJ without assymetry. 
  # The loop is structured as follows:
  # For each year, individuals make a weekly consumption decision based on the specified behavioural model. 
  # These decisions influence assets levels where households either accumulate or fail to accumulate. 
  # At the end of each year, agents update their expected income based on their closing asset levels. 
  for (y in 1:n_years){
   
    if (y == 1){
      for (j in 1:(n_weeks-1)) 
        if (j == 1){ #If the first week in the first year, just assume PIH behaviour to initialise consumption. 
          for (i in 1:n_households){
            consumption_expected = expected_income_KUJ[j,i]
            residual = actual_income_KUJ[j,i] - consumption_expected 
            savings_KUJ[j+1,i] = savings_KUJ[j,i] + residual
            consumption_mat_KUJ[j,i] = consumption_expected
          }
        } else {
          consumption_average = mean(consumption_mat_KUJ[j-1,])
          for (i in 1:n_households){
            consumption_expected = expected_income_KUJ[j,i]
            consumption_difference = consumption_average - consumption_mat_KUJ[j-1,i] #Difference between sample average in j-1 and household i in period j-1
            consumption_KUJ = gamma_mat[j,i] * consumption_difference
            residual = actual_income_KUJ[j,i] - consumption_expected - consumption_KUJ
            savings_KUJ[j+1,i] = savings_KUJ[j,i] + residual
            consumption_mat_KUJ[j,i] = consumption_expected + consumption_KUJ
          }
 
        }
      annual_asset_level_KUJ[y+1,] = savings_KUJ[j+1,] #Save the closing asset levels
      closing_consumption_level[y,] = consumption_mat_KUJ[j,] #Save the average consumption to use for the next year timestep. 
      
    } else {
      # Update expected income based on new asset levels.  
      expected_income_KUJ =  minimum_percapita_income + ((annual_asset_level_KUJ[y,] * interest_rate)/n_weeks)
      expected_income_KUJ = do.call(rbind, replicate(n_weeks, expected_income_KUJ, simplify=FALSE))
      
      # reset the savings matrix to the closing value from the previous year
      savings_KUJ = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings_KUJ[1,] = annual_asset_level_KUJ[y,]
      
      for (i in 1:n_households) {
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, sd_unexpected_inc )
      }
      actual_income_KUJ = expected_income_KUJ +  unexpected_income_mat
      actual_income_KUJ[actual_income_KUJ < 0] = 0
      
      #Rerun the weekly timestep model
      for (j in 1:(n_weeks-1)) {
        if (j == 1){
          consumption_average = mean(closing_consumption_level[y-1,]) # if the first week, retrieve the consumption average from the closing consumption matrix
          for (i in 1:n_households){
            consumption_expected = expected_income_KUJ[j,i]
            consumption_difference = consumption_average - closing_consumption_level[y-1,i] #Difference between sample average in j-1 and household i in period j-1
            consumption_KUJ = gamma_mat[j,i] * consumption_difference
            residual = actual_income_KUJ[j,i] - consumption_expected - consumption_KUJ
            savings_KUJ[j+1,i] = savings_KUJ[j,i] + residual
            consumption_mat_KUJ[j,i] = consumption_expected + consumption_KUJ
          }
        } else {
          consumption_average = mean(consumption_mat_KUJ[j-1,])
          for (i in 1:n_households){
            consumption_expected = expected_income_KUJ[j,i]
            consumption_difference = consumption_average - consumption_mat_KUJ[j-1,i] #Difference between sample average in j-1 and household i in period j-1
            consumption_KUJ = gamma_mat[j,i] * consumption_difference
            residual = actual_income_KUJ[j,i] - consumption_expected - consumption_KUJ
            savings_KUJ[j+1,i] = savings_KUJ[j,i] + residual
            consumption_mat_KUJ[j,i] = consumption_expected + consumption_KUJ

          }
        }
        annual_asset_level_KUJ[y+1,] = savings_KUJ[j+1,] #Save the closing asset levels
        closing_consumption_level[y,] = consumption_mat_KUJ[j,] #Save the average consumption to use for the next year timestep. 
      }
    } 
  }  
 
  
  # KUJ with assymetry. 
  # The loop is structured as follows:
  # For each year, individuals make a weekly consumption decision based on the specified behavioural model. 
  # These decisions influence assets levels where households either accumulate or fail to accumulate. 
  # At the end of each year, agents update their expected income based on their closing asset levels. 
  for (y in 1:n_years){
    
    if (y == 1){
      for (j in 1:(n_weeks-1)) 
        if (j == 1){ #If the first week in the first year, just assume PIH behaviour to initialise consumption. 
          for (i in 1:n_households){
            consumption_expected = expected_income_KUJ_asymmetry[j,i]
            residual = actual_income_KUJ_asymmetry[j,i] - consumption_expected 
            savings_KUJ_asymmetry[j+1,i] = savings_KUJ_asymmetry[j,i] + residual
            consumption_mat_KUJ_asymmetric[j,i] = consumption_expected
          }
        } else {
          consumption_average = mean(consumption_mat_KUJ_asymmetric[j-1,])
          for (i in 1:n_households){
            consumption_expected = expected_income_KUJ_asymmetry[j,i]
            consumption_difference = consumption_average - consumption_mat_KUJ_asymmetric[j-1,i] #Difference between sample average in j-1 and household i in period j-1
            if (consumption_difference > 0) {
            consumption_KUJ_asymmetry = gammapos_mat[j,i] * consumption_difference
            } else {
            consumption_KUJ_asymmetry = gamma_mat[j,i] * consumption_difference
            }
            residual = actual_income_KUJ[j,i] - consumption_expected - consumption_KUJ_asymmetry
            savings_KUJ_asymmetry[j+1,i] = savings_KUJ_asymmetry[j,i] + residual
            consumption_mat_KUJ_asymmetric[j,i] = consumption_expected + consumption_KUJ_asymmetry
          }
          
        }
      annual_asset_level_KUJ_asymmetry[y+1,] = savings_KUJ_asymmetry[j+1,] #Save the closing asset levels
      closing_consumption_level[y,] = consumption_mat_KUJ_asymmetric[j,] #Save the average consumption to use for the next year timestep. 
      
    } else {
      # Update expected income based on new asset levels.  
      expected_income_KUJ_asymmetry =  minimum_percapita_income + ((annual_asset_level_KUJ_asymmetry[y,] * interest_rate)/n_weeks)
      expected_income_KUJ_asymmetry = do.call(rbind, replicate(n_weeks, expected_income_KUJ_asymmetry, simplify=FALSE))
      
      # reset the savings matrix to the closing value from the previous year
      savings_KUJ_asymmetry = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings_KUJ_asymmetry[1,] = annual_asset_level_KUJ_asymmetry[y,]
      
      for (i in 1:n_households) {
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, sd_unexpected_inc )
      }
      actual_income_KUJ_asymmetry = expected_income_KUJ_asymmetry +  unexpected_income_mat
      actual_income_KUJ[actual_income_KUJ_asymmetry < 0] = 0
      
      #Rerun the weekly timestep model
      for (j in 1:(n_weeks-1)) {
        if (j == 1){
          consumption_average = mean(closing_consumption_level[y-1,]) # if the first week, retrieve the consumption average from the closing consumption matrix
          for (i in 1:n_households){
            consumption_expected = expected_income_KUJ_asymmetry[j,i]
            consumption_difference = consumption_average - closing_consumption_level[y-1,i] #Difference between sample average in j-1 and household i in period j-1
            if (consumption_difference > 0) {
              consumption_KUJ_asymmetry = gammapos_mat[j,i] * consumption_difference
            } else {
              consumption_KUJ_asymmetry = gamma_mat[j,i] * consumption_difference
            }
            residual = actual_income_KUJ[j,i] - consumption_expected - consumption_KUJ_asymmetry
            savings_KUJ_asymmetry[j+1,i] = savings_KUJ_asymmetry[j,i] + residual
            consumption_mat_KUJ_asymmetric[j,i] = consumption_expected + consumption_KUJ_asymmetry
          }
        } else {
          consumption_average = mean(consumption_mat_KUJ_asymmetric[j-1,])
          for (i in 1:n_households){
            consumption_expected = expected_income_KUJ_asymmetry[j,i]
            consumption_difference = consumption_average - consumption_mat_KUJ_asymmetric[j-1,i] #Difference between sample average in j-1 and household i in period j-1
            if (consumption_difference > 0) {
              consumption_KUJ_asymmetry = gammapos_mat[j,i] * consumption_difference
            } else {
              consumption_KUJ_asymmetry = gamma_mat[j,i] * consumption_difference
            }
            residual = actual_income_KUJ[j,i] - consumption_expected - consumption_KUJ_asymmetry
            savings_KUJ_asymmetry[j+1,i] = savings_KUJ_asymmetry[j,i] + residual
            consumption_mat_KUJ_asymmetric[j,i] = consumption_expected + consumption_KUJ_asymmetry
          }
        }
        annual_asset_level_KUJ_asymmetry[y+1,] = savings_KUJ_asymmetry[j+1,] #Save the closing asset levels
        closing_consumption_level[y,] = consumption_mat_KUJ_asymmetric[j,] #Save the average consumption to use for the next year timestep. 
      }
    } 
  } 

  return(list(annual_asset_level_KUJ, annual_asset_level_KUJ_asymmetry))
} 