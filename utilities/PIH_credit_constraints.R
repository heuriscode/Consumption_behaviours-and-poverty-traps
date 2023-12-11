# This function generates the simulated model for the permanent income hypothesis with credit constraints
# Where households consume a constant amount and save any windfall (or eat into their savings). 


PIH_credit_constraints = function(deterministic_income, 
                                  actual_income,
                                  savings, 
                                  annual_asset_level,
                                  interest_rate,
                                  minimum_percapita_income,
                                  sd_unexpected_inc, 
                                  min_savings) {
  
  #Retrieve the parameters from the matrix dimensions
  n_households = ncol(deterministic_income)
  n_weeks = nrow(deterministic_income)
  n_years = nrow(annual_asset_level)-1 # the matrix is designed to capture closing asset level for periods 0:n_years
    
  
  #Initialise the weekly consumption matrix. 
  expected_income = matrix(NA, nrow = n_weeks, ncol = n_households)
  consumption = matrix(NA, nrow = n_weeks, ncol = n_households)
  unexpected_income_mat = actual_income - deterministic_income
  
  # Log transform the outputs, retaining the negative values. 
  ihs_func = function(x) {
      y = log((abs(x) + 1000) / 1000)
      y = sign(x) * y
      return(y)
    }
    

  for (y in 1:n_years){
  
    if (y == 1){  #First period behaviour.
      for (i in 1:n_households){
        for (j in 1:(n_weeks-1)){
          consumption[j,i] = deterministic_income[j,i]
          residual = actual_income[j,i] - consumption[j,i]
          savings[j+1,i] = savings[j,i] + residual
          
          if (savings[j+1,i] < min_savings){
            consumption[j,i] = actual_income[j,i]
            savings[j+1,i] = savings[j,i] 
          }
          
        }
      }
      
      annual_asset_level[y+1,] = savings[j+1,] #Save the closing asset levels
    
    } else { 
      
      # Update expected and deterministic income based on new asset levels. 
      deterministic_income = minimum_percapita_income + (annual_asset_level[y,] * interest_rate)/n_weeks
      deterministic_income = do.call(rbind, replicate(n_weeks, deterministic_income, simplify=FALSE))
      
      # Reset the savings matrix with the closing value from the previous period. 
      savings = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings[1,] = annual_asset_level[y,]
      
      # Re generate a randomness to unexpected income and actual income. T
      for (i in 1:n_households) {
        savings_proportion = annual_asset_level[y,i]/ mean(abs(annual_asset_level[1,]))
        if(savings_proportion < 0){
          savings_proportion = 0
        } else if (savings_proportion > 1){
          savings_proportion = 1
        }
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, savings_proportion*sd_unexpected_inc)
      }
      actual_income = deterministic_income + unexpected_income_mat
      actual_income[actual_income<0] = 0
      
        # Now re-run weekly timestep model
        for (i in 1:n_households){
          for (j in 1:(n_weeks-1)){
            consumption[j,i] = deterministic_income[j,i]
            residual = actual_income[j,i] - consumption[j,i]
            savings[j+1,i] = savings[j,i] + residual
            if (savings[j+1,i]<min_savings){
              consumption[j,i] = actual_income[j,i]
              savings[j+1,i] = savings[j,i]
            }
            
          }
        }
      annual_asset_level[y+1,] = savings[j+1,] #Save the closing asset levels 
    }
  } 
 
  #Return the log of annual asset levels for plotting and analysis. 
  annual_asset_level = ihs_func(annual_asset_level)
  return(annual_asset_level)
} 