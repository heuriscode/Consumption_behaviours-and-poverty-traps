# This function generates the simulated model for the permanent income hypothesis with credit constraints
# Where households consume a constant amount and save any windfall (or eat into their savings). 


PIH_credit_constraints = function(expected_income, 
                                  actual_income,
                                  savings, 
                                  annual_asset_level,
                                  interest_rate,
                                  minimum_percapita_income) {
  
  #Retrieve the parameters from the matrix dimensions
  n_households = ncol(expected_income)
  n_weeks = nrow(expected_income)
  n_years = nrow(annual_asset_level)-1 # the matrix is designed to capture closing asset level for periods 0:n_years
  
  #Initialise the weekly consumption matrix. 
  consumption = matrix(NA, nrow = n_weeks, ncol = n_households)
  unexpected_income_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  
  # Log transform the variables - use inverse hyperbolic sinh function to account for zeros. 
  ihs_func = function(x) {
    y = asinh(x/1000)
    return(y)
  }
  
  expected_income = ihs_func(expected_income)
  actual_income = ihs_func(actual_income)
  savings = ihs_func(savings)
  annual_asset_level = ihs_func(annual_asset_level)# But we let households with existing debt to start in debt. 
  minimum_percapita_income = ihs_func(minimum_percapita_income)
  sd_unexpected_inc = sd(expected_income) # As this matrix changes in the loop, pre-define the standard deviation of unexpected income
  

  # The loop is structured as follows:
  # For each year, individuals make a weekly consumption decision based on the specified behavioural model. 
  # These decisions influence assets levels where households either accumulate or fail to accumulate. 
  # At the end of each year, agents update their expected income based on their closing asset levels. 

  for (y in 1:n_years){
  
    if (y == 1){  #First period behaviour.
      for (i in 1:n_households){
        for (j in 1:(n_weeks-1)){
          consumption[j,i] = expected_income[j,i]
          residual = actual_income[j,i] - consumption[j,i]
          savings[j+1,i] = savings[j,i] + residual
        }
      }
      
      annual_asset_level[y+1,] = savings[j+1,] #Save the closing asset levels
    
    } else { 
      # Update expected income based on new asset levels.  
      expected_income = minimum_percapita_income + (annual_asset_level[y,] * interest_rate)/n_weeks
      expected_income = do.call(rbind, replicate(n_weeks, expected_income, simplify=FALSE))
      
      # Reset the savings matrix with the closing value from the previous period. 
      savings = matrix(NA, nrow = n_weeks, ncol = n_households)
      savings[1,] = annual_asset_level[y,]
      
      # Re generate a randomness to unexpected income and actual income. The seed is dynamically but consistently set. 
      set.seed(1000+y)                                              
      for (i in 1:n_households) {
        unexpected_income_mat[,i] = rnorm(n_weeks, 0, sd_unexpected_inc)
      }
      actual_income = expected_income + unexpected_income_mat
      actual_income[actual_income<0] = 0
      
        # Now re-run weekly timestep model
        for (i in 1:n_households){
          for (j in 1:(n_weeks-1)){
            consumption[j,i] = expected_income[j,i]
            residual = actual_income[j,i] - consumption[j,i]
            savings[j+1,i] = savings[j,i] + residual
          }
        }
      annual_asset_level[y+1,] = savings[j+1,] #Save the closing asset levels 
    }
  } 
 
  #Return the annual asset levels for plotting and analysis. 
  return(annual_asset_level)
} 