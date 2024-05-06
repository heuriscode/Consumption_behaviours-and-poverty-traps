# This function generates the simulated model for the Rule of Thumb AND Keeping up with the Joneses'
# Beta matrices contain the individual coefficients for ROT behaviour. 
# Gamma matrices contain the individual coefficients for KUJ behaviour. 
# The function prints out simulated results for the change in savings under symmetric and assymmentric assumptions. 

All_simulation = function(deterministic_income, 
                          actual_income,
                          savings,
                          beta_mat, 
                          betapos_mat,
                          gamma_mat,
                          gammapos_mat,
                          annual_asset_level,
                          interest_rate,
                          minimum_percapita_income,
                          closing_consumption_level, 
                          sd_unexpected_inc, 
                          min_savings) {
  
  #Retrieve the parameters from the matrix dimensions
  n_households = ncol(deterministic_income)
  n_weeks = nrow(deterministic_income)
  n_years = nrow(annual_asset_level)-1 # the matrix is designed to capture closing asset level for periods 0:n_years

  #Initialise the weekly consumption matrix. 
  consumption_mat_All = consumption_mat_All_asymmetric = matrix(NA, nrow = n_weeks, ncol = n_households)
  unexpected_income_mat = actual_income - deterministic_income
  closing_consumption_level_PIH = closing_consumption_level
  
  #Log tranform outputs, taking care of negative values
  ihs_func = function(x) {
    y = log((abs(x) + 1000) / 1000)
    y = sign(x) * y
    return(y)
  }
  
  #Pre-define the matrices to ensure they are consistent with both model runs. 
  savings_All = savings_All_asymmetry = savings
  deterministic_income_All =  deterministic_income_All_asymmetry = deterministic_income
  actual_income_All =  actual_income_All_asymmetry = actual_income
  annual_asset_level_All = annual_asset_level_All_asymmetry = annual_asset_level
  
  # Symmetric Model
  
  # The loop is structured as follows:
  # For each year, individuals make a weekly consumption decision based on the specified behavioural model. 
  # These decisions influence assets levels where households either accumulate or fail to accumulate. 
  # At the end of each year, agents update their expected income based on their closing asset levels. 
  for (y in 1:n_years){
   
    if (y == 1){
      
      for (j in 1:(n_weeks-1)) 
        if (j == 1){ #If the first week in the first year, just assume PIH and ROT behaviour to initialise consumption. 
          for (i in 1:n_households){
            consumption_expected = deterministic_income_All[j,i]
            consumption_ROT = beta_mat[j,i] * unexpected_income_mat[j,i]
            residual = actual_income_All[j,i] - consumption_expected - consumption_ROT
            savings_All[j+1,i] = savings_All[j,i] + residual
            consumption_mat_All[j,i] = consumption_expected - consumption_ROT
            
            # min savings threshold
            if (savings_All[j+1,i] < min_savings){
              consumption_expected = actual_income_All[j,i]
              savings_All[j+1,i] = savings_All[j,i]
              consumption_mat_All[j,i] = consumption_expected
            }
            
          }
        } else {
          
          consumption_average =  mean(deterministic_income_All[j-1,]) # assume parish peers follow PIH with expected income
          
          for (i in 1:n_households){
            #PIH
            consumption_expected = deterministic_income_All[j,i]
            #ROT
            consumption_ROT = beta_mat[j,i] * unexpected_income_mat[j,i]
            #KUJ
            consumption_difference = consumption_average - consumption_mat_All[j-1,i] #Difference between sample average in j-1 and household i in period j-1
            consumption_KUJ = gamma_mat[j,i] * consumption_difference
            #TOTAL
            residual = actual_income_All[j,i] - consumption_expected - consumption_ROT - consumption_KUJ
            savings_All[j+1,i] = savings_All[j,i] + residual
            consumption_mat_All[j,i] = consumption_expected +  consumption_ROT + consumption_KUJ 
            
            # min savings threshold
            if (savings_All[j+1,i] < min_savings){
              consumption_expected = actual_income_All[j,i]
              savings_All[j+1,i] = savings_All[j,i]
              consumption_mat_All[j,i] = consumption_expected
            }
          }
 
        }
      annual_asset_level_All[y+1,] = savings_All[j+1,] #Save the closing asset levels
      closing_consumption_level[y,] = consumption_mat_All[j,] #Save the average consumption to use for the next year timestep. 
      closing_consumption_level_PIH[y,] = deterministic_income_All[j,] #Save the PIH consumption to use for the next year timestep. 
      
    } else {
      
      # Update expected income based on new asset levels.  
      deterministic_income_All =  minimum_percapita_income + ((annual_asset_level_All[y,] * interest_rate)/n_weeks)
      deterministic_income_All = do.call(rbind, replicate(n_weeks, deterministic_income_All, simplify=FALSE))
      
      # reset the savings matrix to the closing value from the previous year
      savings_All = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings_All[1,] = annual_asset_level_All[y,]
      
      # Retrieve unexpected income and actual income
      for (i in 1:n_households) {
        savings_proportion = annual_asset_level_All[y,i]/ mean(abs(annual_asset_level_All[1,]))
        if(savings_proportion < 0){
          savings_proportion = 0
        } else if (savings_proportion > 1){
          savings_proportion = 1
        }
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, savings_proportion*sd_unexpected_inc)
      }
      
      actual_income_All = deterministic_income_All + unexpected_income_mat
      actual_income_All[actual_income_All < 0] = 0
      
      #Rerun the weekly timestep model
      for (j in 1:(n_weeks-1)) {
        if (j == 1){
          
          consumption_average = mean(closing_consumption_level_PIH[y-1,]) # if the first week, retrieve the consumption average from the closing consumption matrix
          
          for (i in 1:n_households){
            #PIH
            consumption_expected = deterministic_income_All[j,i]
            #ROT
            consumption_ROT = beta_mat[j,i] * unexpected_income_mat[j,i]
            #KUJ
            consumption_difference = consumption_average - closing_consumption_level[y-1,i] #Difference between sample average in j-1 and household i in period j-1
            consumption_KUJ = gamma_mat[j,i] * consumption_difference
            #TOTAL
            residual = actual_income_All[j,i] - consumption_expected - consumption_ROT - consumption_KUJ 
            savings_All[j+1,i] = savings_All[j,i] + residual
            consumption_mat_All[j,i] = consumption_expected +  consumption_ROT + consumption_KUJ 
            
            # min savings threshold
            if (savings_All[j+1,i] < min_savings){
              consumption_expected = actual_income_All[j,i]
              savings_All[j+1,i] = savings_All[j,i]
              consumption_mat_All[j,i] = consumption_expected
            }
          }
          
        } else {
          
          consumption_average = mean(deterministic_income_All[j-1,]) # assume parish peers follow PIH with expected income
          
          for (i in 1:n_households){
            #PIH
            consumption_expected = deterministic_income_All[j,i]
            #ROT
            consumption_ROT = beta_mat[j,i] * unexpected_income_mat[j,i]
            #KUJ
            consumption_difference = consumption_average - consumption_mat_All[j-1,i] #Difference between sample average in j-1 and household i in period j-1
            consumption_KUJ = gamma_mat[j,i] * consumption_difference
            #TOTAL
            residual = actual_income_All[j,i] - consumption_expected - consumption_ROT - consumption_KUJ 
            savings_All[j+1,i] = savings_All[j,i] + residual
            consumption_mat_All[j,i] = consumption_expected +  consumption_ROT + consumption_KUJ 
            
            # min savings threshold
            if (savings_All[j+1,i] < min_savings){
              consumption_expected = actual_income_All[j,i]
              savings_All[j+1,i] = savings_All[j,i]
              consumption_mat_All[j,i] = consumption_expected
            }

          }
        }
        annual_asset_level_All[y+1,] = savings_All[j+1,] #Save the closing asset levels
        closing_consumption_level[y,] = consumption_mat_All[j,] #Save the average consumption to use for the next year timestep. 
        closing_consumption_level_PIH[y,] = deterministic_income_All[j,] #Save the PIH consumption to use for the next year timestep. 
      }
    } 
  }  
 
  
  # Asymmetric Model
  
  # The loop is structured as follows:
  # For each year, individuals make a weekly consumption decision based on the specified behavioural model. 
  # These decisions influence assets levels where households either accumulate or fail to accumulate. 
  # At the end of each year, agents update their expected income based on their closing asset levels. 
  for (y in 1:n_years){
    
    if (y == 1){
      for (j in 1:(n_weeks-1)) 
        if (j == 1){ #If the first week in the first year, just assume PIH and ROT behaviour to initialise consumption
          for (i in 1:n_households){
            #PIH
            consumption_expected = deterministic_income_All_asymmetry[j,i]
            #ROT
            if (unexpected_income_mat[j,i] > 0){
              consumption_ROT_asymmetry = betapos_mat[j,i] * unexpected_income_mat[j,i]
            } else {
              consumption_ROT_asymmetry = beta_mat[j,i] * unexpected_income_mat[j,i]
            }
            #TOTAL
            residual = actual_income_All_asymmetry[j,i] - consumption_expected - consumption_ROT_asymmetry
            savings_All_asymmetry[j+1,i] = savings_All_asymmetry[j,i] + residual
            consumption_mat_All_asymmetric[j,i] = consumption_expected + consumption_ROT_asymmetry
            
            # min savings rule
            if (savings_All_asymmetry[j+1,i] < min_savings){
              consumption_expected = actual_income_All_asymmetry[j,i]
              savings_All_asymmetry[j+1,i] = savings_All_asymmetry[j,i] 
              consumption_mat_All_asymmetric[j,i] = consumption_expected
            }
            
          }
        } else {
          
          consumption_average = mean(deterministic_income_All_asymmetry[j-1,])
          for (i in 1:n_households){
            #PIH
            consumption_expected = deterministic_income_All_asymmetry[j,i]
            #ROT 
            if (unexpected_income_mat[j,i] > 0){
              consumption_ROT_asymmetry = betapos_mat[j,i] * unexpected_income_mat[j,i]
            } else {
              consumption_ROT_asymmetry = beta_mat[j,i] * unexpected_income_mat[j,i]
            }
            #KUJ
            consumption_difference = consumption_average - consumption_mat_All_asymmetric[j-1,i] #Difference between sample average in j-1 and household i in period j-1
            if (consumption_difference > 0) {
            consumption_KUJ_asymmetry = gammapos_mat[j,i] * consumption_difference
            } else {
            consumption_KUJ_asymmetry = gamma_mat[j,i] * consumption_difference
            }
            #TOTAL
            residual = actual_income_All[j,i] - consumption_expected - consumption_KUJ_asymmetry - consumption_ROT_asymmetry
            savings_All_asymmetry[j+1,i] = savings_All_asymmetry[j,i] + residual
            consumption_mat_All_asymmetric[j,i] = consumption_expected + consumption_KUJ_asymmetry + consumption_ROT_asymmetry
            
            # min savings rule
            if (savings_All_asymmetry[j+1,i] < min_savings){
              consumption_expected = actual_income_All_asymmetry[j,i]
              savings_All_asymmetry[j+1,i] = savings_All_asymmetry[j,i] 
              consumption_mat_All_asymmetric[j,i] = consumption_expected
            }
          }
        }
      
      annual_asset_level_All_asymmetry[y+1,] = savings_All_asymmetry[j+1,] #Save the closing asset levels
      closing_consumption_level[y,] = consumption_mat_All_asymmetric[j,] #Save the average consumption to use for the next year timestep. 
      closing_consumption_level_PIH[y,] = deterministic_income_All_asymmetry[j,] #Save the PIH consumption to use for the next year timestep. 
    } else {
      
      # Update expected income based on new asset levels.  
      deterministic_income_All_asymmetry =  minimum_percapita_income + ((annual_asset_level_All_asymmetry[y,] * interest_rate)/n_weeks)
      deterministic_income_All_asymmetry = do.call(rbind, replicate(n_weeks, deterministic_income_All_asymmetry, simplify=FALSE))
      
      # reset the savings matrix to the closing value from the previous year
      savings_All_asymmetry = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings_All_asymmetry[1,] = annual_asset_level_All_asymmetry[y,]
      
      # Retrieve unexpected income and actual income
      for (i in 1:n_households) {
        savings_proportion = annual_asset_level_All_asymmetry[y,i]/ mean(abs(annual_asset_level_All_asymmetry[1,]))
        if(savings_proportion < 0){
          savings_proportion = 0
        } else if (savings_proportion > 1){
          savings_proportion = 1
        }
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, savings_proportion*sd_unexpected_inc)
      }
      
      actual_income_All_asymmetry = deterministic_income_All_asymmetry +  unexpected_income_mat
      actual_income_All[actual_income_All_asymmetry < 0] = 0
      
      #Rerun the weekly timestep model
      for (j in 1:(n_weeks-1)) {
        if (j == 1){
          consumption_average = mean(closing_consumption_level_PIH[y-1,]) # if the first week, retrieve the consumption average from the closing consumption matrix
          
          for (i in 1:n_households){
            #PIH
            consumption_expected = deterministic_income_All_asymmetry[j,i]
            #ROT
            if (unexpected_income_mat[j,i] > 0){
              consumption_ROT_asymmetry = betapos_mat[j,i] * unexpected_income_mat[j,i]
            } else {
              consumption_ROT_asymmetry = beta_mat[j,i] * unexpected_income_mat[j,i]
            }
            #KUJ 
            consumption_difference = consumption_average - closing_consumption_level[y-1,i] #Difference between sample average in j-1 and household i in period j-1
            if (consumption_difference > 0) {
              consumption_KUJ_asymmetry = gammapos_mat[j,i] * consumption_difference
            } else {
              consumption_KUJ_asymmetry = gamma_mat[j,i] * consumption_difference
            }
            #TOTAL
            residual = actual_income_All[j,i] - consumption_expected - consumption_KUJ_asymmetry - consumption_ROT_asymmetry
            savings_All_asymmetry[j+1,i] = savings_All_asymmetry[j,i] + residual
            consumption_mat_All_asymmetric[j,i] = consumption_expected + consumption_KUJ_asymmetry + consumption_ROT_asymmetry
            
            # min savings rule
            if (savings_All_asymmetry[j+1,i] < min_savings){
              consumption_expected = actual_income_All_asymmetry[j,i]
              savings_All_asymmetry[j+1,i] = savings_All_asymmetry[j,i] 
              consumption_mat_All_asymmetric[j,i] = consumption_expected
            }
          }
        } else {
          
          consumption_average = mean(deterministic_income_All_asymmetry[j-1,])
          
          for (i in 1:n_households){
            #PIH
            consumption_expected = deterministic_income_All_asymmetry[j,i]
            #ROT
            if (unexpected_income_mat[j,i] > 0){
              consumption_ROT_asymmetry = betapos_mat[j,i] * unexpected_income_mat[j,i]
            } else {
              consumption_ROT_asymmetry = beta_mat[j,i] * unexpected_income_mat[j,i]
            }
            #KUJ 
            consumption_difference = consumption_average - consumption_mat_All_asymmetric[j-1,i] #Difference between sample average in j-1 and household i in period j-1
            if (consumption_difference > 0) {
              consumption_KUJ_asymmetry = gammapos_mat[j,i] * consumption_difference
            } else {
              consumption_KUJ_asymmetry = gamma_mat[j,i] * consumption_difference
            }
            #TOTAL
            residual = actual_income_All[j,i] - consumption_expected - consumption_KUJ_asymmetry - consumption_ROT_asymmetry
            savings_All_asymmetry[j+1,i] = savings_All_asymmetry[j,i] + residual
            consumption_mat_All_asymmetric[j,i] = consumption_expected + consumption_KUJ_asymmetry + consumption_ROT_asymmetry
            # min savings rule
            if (savings_All_asymmetry[j+1,i] < min_savings){
              consumption_expected = actual_income_All_asymmetry[j,i]
              savings_All_asymmetry[j+1,i] = savings_All_asymmetry[j,i] 
              consumption_mat_All_asymmetric[j,i] = consumption_expected
            }
          }
        }
        annual_asset_level_All_asymmetry[y+1,] = savings_All_asymmetry[j+1,] #Save the closing asset levels
        closing_consumption_level[y,] = consumption_mat_All_asymmetric[j,] #Save the average consumption to use for the next year timestep. 
        closing_consumption_level_PIH[y,] = deterministic_income_All_asymmetry[j,] #Save the average consumption to use for the next year timestep. 
      }
    } 
  } 
  
  #Log outputs
  annual_asset_level_All = ihs_func(annual_asset_level_All)
  annual_asset_level_All_asymmetry = ihs_func(annual_asset_level_All_asymmetry)
  
  return(list(annual_asset_level_All, annual_asset_level_All_asymmetry))
} 