# This function generates the simulated model for the Rule of Thumb consumption. 
# Beta matrices contain the individual coefficients for ROT behaviour. 
# The function prints out simulated results for the change in savings under ROT, and assymetric ROT. 

ROT_simulation = function(expected_income, 
                          actual_income,
                          savings,
                          beta_mat, 
                          betapos_mat,
                          annual_asset_level,
                          interest_rate,
                          minimum_percapita_income) {
  
  #Retrieve the parameters from the matrix dimensions
  n_households = ncol(expected_income)
  n_weeks = nrow(expected_income)
  n_years = nrow(annual_asset_level)-1 # the matrix is designed to capture closing asset level for periods 0:n_years
 
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
  

  #Initialise the weekly consumption matrix. 
  consumption = matrix(NA, nrow = n_weeks, ncol = n_households)
  unexpected_income_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  
  #Pre-define the matrices to ensure they are consistent with both model runs. 
  savings_ROT = savings_ROT_assymetric = savings
  expected_income_ROT =  expected_income_ROT_assymetric = expected_income
  actual_income_ROT =  actual_income_ROT_assymetric = actual_income
  annual_asset_level_ROT = annual_asset_level_ROT_assymetric = annual_asset_level

  
  #ROT without assymetry. 
  # The loop is structured as follows:
  # For each year, individuals make a weekly consumption decision based on the specified behavioural model. 
  # These decisions influence assets levels where households either accumulate or fail to accumulate. 
  # At the end of each year, agents update their expected income based on their closing asset levels. 
  for (y in 1:n_years){
   
    if (y == 1){
       for (i in 1:n_households){
          for (j in 1:(n_weeks-1)){
            consumption_expected = expected_income_ROT[j,i]
            consumption_ROT = beta_mat[j,i] * (actual_income_ROT[j,i] - expected_income_ROT[j,i]) 
            residual = actual_income_ROT[j,i] - consumption_expected - consumption_ROT 
            savings_ROT[j+1,i] = savings_ROT[j,i] + residual
          }
       }
      annual_asset_level_ROT[y+1,] = savings_ROT[j+1,] #Save the closing asset levels
    } else {
      # Update expected income based on new asset levels.  
      expected_income_ROT =  minimum_percapita_income + ((annual_asset_level_ROT[y,] * interest_rate)/n_weeks)
      expected_income_ROT = do.call(rbind, replicate(n_weeks, expected_income_ROT, simplify=FALSE))
      
      # reset the savings matrix to the closing value from the previous year
      savings_ROT = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings_ROT[1,] = annual_asset_level_ROT[y,]
      
      for (i in 1:n_households) {
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, sd_unexpected_inc )
      }
      actual_income_ROT = expected_income_ROT +  unexpected_income_mat
      actual_income_ROT[actual_income_ROT < 0] = 0
      
      #Rerun the weekly timestep model
      for (i in 1:n_households){
        for (j in 1:(n_weeks-1)){
          consumption_expected = expected_income_ROT[j,i]
          consumption_ROT = beta_mat[j,i] * (actual_income_ROT[j,i] - expected_income_ROT[j,i]) 
          residual = actual_income_ROT[j,i] - consumption_expected - consumption_ROT 
          savings_ROT[j+1,i] = savings_ROT[j,i] + residual
        }
      }
      annual_asset_level_ROT[y+1,] = savings_ROT[j+1,] #Save the closing asset levels
    }
  } 
  
  
  # ROT with assymetry. 
  # This loop follows the same structure, but recognizes asymmetry in the ROT response to positive and negative shocks. 
  for (y in 1:n_years){
    
    if (y == 1){
      for (i in 1:n_households){
        for (j in 1:(n_weeks-1)){
          consumption_expected = expected_income_ROT_assymetric[j,i]
          if (actual_income_ROT_assymetric[j,i] - expected_income_ROT_assymetric[j,i] > 0){
            consumption_ROT = betapos_mat[j,i] * (actual_income_ROT_assymetric[j,i] - expected_income_ROT_assymetric[j,i]) 
            residual = actual_income_ROT_assymetric[j,i] - consumption_expected - consumption_ROT 
            savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j,i] + residual
          } else {
            consumption_ROT = beta_mat[j,i] * (actual_income_ROT_assymetric[j,i] - expected_income_ROT_assymetric[j,i]) 
            residual = actual_income_ROT_assymetric[j,i] - consumption_expected - consumption_ROT 
            savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j,i] + residual
          }
          
        }
      }
      annual_asset_level_ROT_assymetric[y+1,] = savings_ROT_assymetric[j+1,] #Save the closing asset levels
      
    } else {
      
      # Update expected income based on new asset levels.  
      expected_income_ROT_assymetric = minimum_percapita_income + ((annual_asset_level_ROT_assymetric[y,] * interest_rate)/n_weeks)
      expected_income_ROT_assymetric = do.call(rbind, replicate(n_weeks, expected_income_ROT_assymetric, simplify=FALSE))
      
      # reset the savings matrix to the closing value from the previous year
      savings_ROT_assymetric = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings_ROT_assymetric[1,] = annual_asset_level_ROT_assymetric[y,]
      
      # Re generate a randomness to unexpected income and actual income. 
      for (i in 1:n_households) {
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, sd_unexpected_inc )
      }
      actual_income_ROT_assymetric = expected_income_ROT_assymetric + unexpected_income_mat
      actual_income_ROT_assymetric[actual_income_ROT_assymetric < 0] = 0
      
      #Rerun the weekly timestep model
      for (i in 1:n_households){
        for (j in 1:(n_weeks-1)){
          consumption_expected = expected_income_ROT_assymetric[j,i]
          if (actual_income_ROT_assymetric[j,i] - expected_income_ROT_assymetric[j,i] > 0){
            consumption_ROT = betapos_mat[j,i] * (actual_income_ROT_assymetric[j,i] - expected_income_ROT_assymetric[j,i]) 
            residual = actual_income_ROT_assymetric[j,i] - consumption_expected - consumption_ROT 
            savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j,i] + residual
          } else {
            consumption_ROT = beta_mat[j,i] * (actual_income_ROT_assymetric[j,i] - expected_income_ROT_assymetric[j,i]) 
            residual = actual_income_ROT_assymetric[j,i] - consumption_expected - consumption_ROT 
            savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j,i] + residual
          }
          
        }
      }
      annual_asset_level_ROT_assymetric[y+1,] = savings_ROT_assymetric[j+1,] #Save the closing asset levels
    }
  }
  
  return(list(annual_asset_level_ROT, annual_asset_level_ROT_assymetric))
} 