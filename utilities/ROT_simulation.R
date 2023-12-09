# This function generates the simulated model for the Rule of Thumb consumption. 
# Beta matrices contain the individual coefficients for ROT behaviour. 
# The function prints out simulated results for the change in savings under ROT, and assymetric ROT. 

ROT_simulation = function(deterministic_income, 
                          actual_income,
                          savings,
                          beta_mat, 
                          betapos_mat,
                          annual_asset_level,
                          interest_rate,
                          minimum_percapita_income,
                          sd_unexpected_inc,
                          min_savings) {
  

  #Retrieve the parameters from the matrix dimensions
  n_households = ncol(deterministic_income)
  n_weeks = nrow(deterministic_income)
  n_years = nrow(annual_asset_level)-1 # the matrix is designed to capture closing asset level for periods 0:n_years
 
  ihs_func = function(x) {
    y = log((abs(x) + 1000) / 1000)
    y = sign(x) * y
    return(y)
  }
  
  
  #Initialise the weekly consumption matrix. 
  consumption = matrix(NA, nrow = n_weeks, ncol = n_households)
  unexpected_income_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  
  #Pre-define the matrices to ensure they are consistent with both model runs. 
  savings_ROT = savings_ROT_assymetric = savings
  deterministic_income_ROT =  deterministic_income_ROT_assymetric = deterministic_income
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
            consumption_expected = deterministic_income_ROT[j,i]
            consumption_ROT = beta_mat[j,i] * (actual_income_ROT[j,i] - deterministic_income_ROT[j,i]) 
            residual = actual_income_ROT[j,i] - consumption_expected - consumption_ROT 
            savings_ROT[j+1,i] = savings_ROT[j,i] + residual
            
            #min savings rule
            if (savings_ROT[j+1,i] < min_savings){
              consumption_ROT  = actual_income_ROT[j,i]
              savings_ROT[j+1,i] = savings_ROT[j,i]
            }
            
          }
       }
      annual_asset_level_ROT[y+1,] = savings_ROT[j+1,] #Save the closing asset levels
    } else {
      
      # Update deterministic income based on new asset levels.  
      deterministic_income_ROT =  minimum_percapita_income + ((annual_asset_level_ROT[y,] * interest_rate)/n_weeks)
      deterministic_income_ROT = do.call(rbind, replicate(n_weeks, deterministic_income_ROT, simplify=FALSE))
      
      # reset the savings matrix to the closing value from the previous year
      savings_ROT = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings_ROT[1,] = annual_asset_level_ROT[y,]
      
      # Retrieve unexpected income and actual income
      for (i in 1:n_households) {
        savings_proportion = annual_asset_level_ROT[y,i]/ mean(abs(annual_asset_level_ROT[1,]))
        if(savings_proportion < 0){
          savings_proportion = 0
        } else if (savings_proportion > 1){
          savings_proportion = 1
        }
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, savings_proportion*sd_unexpected_inc)
      }
      actual_income_ROT = deterministic_income_ROT +  unexpected_income_mat
      actual_income_ROT[actual_income_ROT < 0] = 0
      
      #Rerun the weekly timestep model
      for (i in 1:n_households){
        for (j in 1:(n_weeks-1)){
          consumption_expected = deterministic_income_ROT[j,i]
          consumption_ROT = beta_mat[j,i] * (actual_income_ROT[j,i] - deterministic_income_ROT[j,i]) 
          residual = actual_income_ROT[j,i] - consumption_expected - consumption_ROT 
          savings_ROT[j+1,i] = savings_ROT[j,i] + residual
         
          #min savings rule
          if (savings_ROT[j+1,i] < min_savings){
            consumption_ROT  = actual_income_ROT[j,i]
            savings_ROT[j+1,i] = savings_ROT[j,i]
          }
          
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
          consumption_expected = deterministic_income_ROT_assymetric[j,i]
          if (actual_income_ROT_assymetric[j,i] - deterministic_income_ROT_assymetric[j,i] > 0){
            consumption_ROT = betapos_mat[j,i] * (actual_income_ROT_assymetric[j,i] - deterministic_income_ROT_assymetric[j,i]) 
            residual = actual_income_ROT_assymetric[j,i] - consumption_expected - consumption_ROT 
            savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j,i] + residual
            
            #min savings rule
            if (savings_ROT_assymetric[j+1,i] < min_savings){
              consumption_ROT = actual_income_ROT_assymetric[j,i]
              savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j+1,i]
            }
            
            
          } else {
            consumption_ROT = beta_mat[j,i] * (actual_income_ROT_assymetric[j,i] - deterministic_income_ROT_assymetric[j,i]) 
            residual = actual_income_ROT_assymetric[j,i] - consumption_expected - consumption_ROT 
            savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j,i] + residual
            
            #min savings rule
            if (savings_ROT_assymetric[j+1,i] < min_savings){
              consumption_ROT = actual_income_ROT_assymetric[j,i]
              savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j+1,i]
            }
            
          }
          
        }
      }
      annual_asset_level_ROT_assymetric[y+1,] = savings_ROT_assymetric[j+1,] #Save the closing asset levels
      
    } else {
      
      # Update expected income based on new asset levels.  
      deterministic_income_ROT_assymetric = minimum_percapita_income + ((annual_asset_level_ROT_assymetric[y,] * interest_rate)/n_weeks)
      deterministic_income_ROT_assymetric = do.call(rbind, replicate(n_weeks, deterministic_income_ROT_assymetric, simplify=FALSE))
      
      # reset the savings matrix to the closing value from the previous year
      savings_ROT_assymetric = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings_ROT_assymetric[1,] = annual_asset_level_ROT_assymetric[y,]
      
      # Re generate a randomness to unexpected income and actual income. 
      for (i in 1:n_households) {
        savings_proportion = annual_asset_level_ROT_assymetric[y,i]/ mean(abs(annual_asset_level_ROT_assymetric[1,]))
        if(savings_proportion < 0){
          savings_proportion = 0
        } else if (savings_proportion > 1){
          savings_proportion = 1
        }
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, savings_proportion*sd_unexpected_inc)
      }
      actual_income_ROT_assymetric = deterministic_income_ROT_assymetric + unexpected_income_mat
      actual_income_ROT_assymetric[actual_income_ROT_assymetric < 0] = 0
      
      #Rerun the weekly timestep model
      for (i in 1:n_households){
        for (j in 1:(n_weeks-1)){
          consumption_expected = deterministic_income_ROT_assymetric[j,i]
          if (actual_income_ROT_assymetric[j,i] - deterministic_income_ROT_assymetric[j,i] > 0){
            consumption_ROT = betapos_mat[j,i] * (actual_income_ROT_assymetric[j,i] - deterministic_income_ROT_assymetric[j,i]) 
            residual = actual_income_ROT_assymetric[j,i] - consumption_expected - consumption_ROT 
            savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j,i] + residual
            
            #min savings rule
            if (savings_ROT_assymetric[j+1,i] < min_savings){
              consumption_ROT = actual_income_ROT_assymetric[j,i]
              savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j+1,i]
            }
            
          } else {
            consumption_ROT = beta_mat[j,i] * (actual_income_ROT_assymetric[j,i] - deterministic_income_ROT_assymetric[j,i]) 
            residual = actual_income_ROT_assymetric[j,i] - consumption_expected - consumption_ROT 
            savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j,i] + residual
            
            #min savings rule
            if (savings_ROT_assymetric[j+1,i] < min_savings){
              consumption_ROT = actual_income_ROT_assymetric[j,i]
              savings_ROT_assymetric[j+1,i] = savings_ROT_assymetric[j+1,i]
            }
          }
          
        }
      }
      annual_asset_level_ROT_assymetric[y+1,] = savings_ROT_assymetric[j+1,] #Save the closing asset levels
    }
  }
  
  annual_asset_level_ROT = ihs_func(annual_asset_level_ROT)
  annual_asset_level_ROT_assymetric - ihs_func(annual_asset_level_ROT_assymetric)
  return(list(annual_asset_level_ROT, annual_asset_level_ROT_assymetric))
} 