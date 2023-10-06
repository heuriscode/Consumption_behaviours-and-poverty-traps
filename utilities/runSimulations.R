# This script runs the wealth dynamics simulations. 
# The first part of the script retrieves the parameters and initial conditions 
# Then it calls individual functions that perform the simulations
# And then returns plots of lagged and current savings against the 45 degree line.

runSimulations = function(bg_data, data, rotlist, joneslist) {
  #Parameters
  n_households = 1000
  n_weeks = 52
  n_parishes = 1000 #Number of parishes for keeping up with the joneses. 
  n_years = 25 # Number of years we observe the behaviour. 
  
  # The expected income definition is a minimum per capita income + return on per capita assets. 
  # First we will determine an appropriate definition of minimum per capita income and an interest rate
  # This is done to approximately match the distribution of average per capita income per week. 
  # This occurs at an interest rate for assets (net debts) of 10%, 
  # And a minimum per capita income of 7000 UGX per week. 
  # Below we confirm this with the histograms. 
  interest_rate = 0.25
  minimum_percapita_income = 9000
  
  total_assets_percapita = rowSums(cbind(bg_data$VAL_ASSETS_BICYCLE,
                                         bg_data$VAL_ASSETS_BODA,
                                         bg_data$VAL_ASSETS_SAVINGS,
                                         bg_data$VAL_ASSETS_SMARTPHONE,
                                         bg_data$VAL_ASSETS_TV,
                                         bg_data$DEBTS_CAR_LOAN,
                                         bg_data$DEBTS_FAMILY,
                                         bg_data$DEBTS_FRIEND,
                                         bg_data$DEBTS_OTHER_LOAN, na.rm = TRUE))/bg_data$NUM_IN_HOUSEHOLD
  
  #Trim outliers
  tails = quantile(total_assets_percapita, probs=c(0, .95), na.rm = TRUE)
  
  
  dividends = subset(total_assets_percapita, total_assets_percapita > tails[1] & total_assets_percapita < tails[2]) * interest_rate
  average_percapita_income_estimated = dividends/n_weeks + minimum_percapita_income 
  
  income_unscaled = data$MONEY_EARNT_PICKING + 
    data$MONEY_EARNT_COFF_SALES + 
    data$MONEY_EARNT_PARCHMENT + 
    data$MONEY_EARNT_OTHER_WORK +
    data$MONEY_EARNT_GOVT_NGO + 
    data$MONEY_EARNT_PARTNER + 
    replace(data$GAMBLING,is.na(data$GAMBLING),0)
  
  average_percapita_income = numeric(nrow(bg_data))
  for (i in 1:nrow(bg_data)){
    ID = bg_data$ID
    bg_data$NUM_IN_HOUSEHOLD[i]
    average_percapita_income[i] = sum(income_unscaled[which(data$SURVEY_ID == ID)]) / (bg_data$NUM_IN_HOUSEHOLD[i] * 26)
  }
  
  # Confirm with histograms.
  par(mfrow=c(1,1))
  hist(log(average_percapita_income_estimated))
  hist(log(average_percapita_income))
  
  # Expected income will be defined by this minimum per capita income and return on assets. 
  # This requires a distribution of per capita assets. 
  
  set.seed(123) #Seed re-determined for reproducability but can be changed. This is adjusted in the script below for each random draw. 
  
  #Remove large outliers for the savings and determine aggregate savings per capita.
  net_savings_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  mean_net_starting_assets = mean(subset(total_assets_percapita, total_assets_percapita > tails[1] & total_assets_percapita < tails[2]))
  sd_net_starting_assets = sd(subset(total_assets_percapita, total_assets_percapita > tails[1] & total_assets_percapita < tails[2]))
  for (i in 1:n_households) {
    net_savings_mat[1,i] = rnorm(1, mean_net_starting_assets, sd_net_starting_assets) 
  } 
  
  
  expected_income_mat = minimum_percapita_income + (net_savings_mat[1,] * interest_rate)/n_weeks
  expected_income_mat = do.call(rbind, replicate(n_weeks, expected_income_mat , simplify=FALSE))
  
  # Have a stochastically determined random shock drawn from a mean and variance of change in unexpected income from the results. This is unexpected income
  # Variation in income appears to trend towards a stable point of 0 at higher income levels, suggesting lower risk investments. 
  # When compared to assets, variation appears somewhat stable around 0. 
  # Because of this, we can assume variation in income/assets is relatively constant for the model. 
  # We will use the sd of expected income for unexpected income
  # We based this on the s..e of RESID_INC being approximate to the s.e. of EXPECT_INC in the modelling. 
  
  set.seed(456) 
  unexpected_income_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  for (i in 1:n_households) {
    unexpected_income_mat[,i] = rnorm(n_weeks, 0, sd(expected_income_mat))
  }
  
  # Sum this with mean income to give actual income. If negative, code as 0
  actual_income_mat = expected_income_mat +  unexpected_income_mat
  actual_income_mat[actual_income_mat<0] = 0
  
  # Draw from the results alpha, from a distribution with mean equal to the coefficient and sd equal to the standard error.
  # alpha represents the responsiveness of consumption to changes in expected income.
  # truncate at zero
  set.seed(789)
  alpha_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  alpha_mean = summary(rotlist[[2]])$CoefTable[1,1]
  alpha_sd = summary(rotlist[[2]])$CoefTable[1,2]
  for (i in 1:n_households) {
    alpha_mat[,i] = rnorm(1, alpha_mean,alpha_sd ) #Outmat_rot contains the result matrix
  }
  alpha_mat[alpha_mat<0] = 0
  
  # Draw alpha for positive shocks to expected income, from a distribution with mean equal to the coefficient and sd equal to the standard error.
  # alpha_pos represents the responsiveness of postiive consumption to changes in expected income.
  # truncate at zero
  set.seed(135)
  alphapos_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  alphapos_mean = summary(rotlist[[2]])$CoefTable[2,1] + summary(rotlist[[1]])$CoefTable[1,1]
  alphapos_rho = rotlist[[2]]$vcov[1,2]
  alphapos_sd = sqrt(summary(rotlist[[2]])$CoefTable[2,2]^2 + summary(rotlist[[2]])$CoefTable[1,2]^2 + 2 * alphapos_rho * summary(rotlist[[2]])$CoefTable[2,2] * summary(rotlist[[2]])$CoefTable[1,2])
  for (i in 1:n_households) {
    alphapos_mat[,i] = rnorm(1, alphapos_mean, alphapos_sd )
  }
  alphapos_mat[alphapos_mat<0] = 0
  
  # Draw from the results beta, from a distribution with mean equal to the coefficient and sd equal to the standard error.
  # beta represents the responsiveness of consumption to changes in unexpected income under RULE OF THUMB. 
  set.seed(246)
  beta_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  beta_mean = summary(rotlist[[2]])$CoefTable[3,1]
  beta_sd = summary(rotlist[[2]])$CoefTable[3,2]
  for (i in 1:n_households) {
    beta_mat[,i] = rnorm(1, beta_mean, beta_sd ) 
  }
  
  # Draw from the results beta for positive unexpected shocks, from a distribution with mean equal to the coefficient and sd equal to the standard error.
  # beta represents the responsiveness of consumption to changes in positive unexpected income under RULE OF THUMB. 
  set.seed(357)
  betapos_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  betapos_mean = summary(rotlist[[2]])$CoefTable[3,1]+ summary(rotlist[[2]])$CoefTable[4,1]
  betapos_rho = rotlist[[2]]$vcov[4,3] 
  betapos_sd = sqrt(summary(rotlist[[2]])$CoefTable[3,2]^2 + summary(rotlist[[2]])$CoefTable[4,2]^2 + 2 * betapos_rho * summary(rotlist[[2]])$CoefTable[3,2] * summary(rotlist[[2]])$CoefTable[4,2])
  for (i in 1:n_households) {
    betapos_mat[,i] = rnorm(1, betapos_mean, betapos_sd ) 
  }
  
  # # For Keeping up with the Joneses - randomly map households to parishes. 
  # # Create a vector of parish assignments for households
  # parish_assignments = rep(1:n_parishes, each = floor(n_households/n_parishes))
  # remaining_households = n_households - length(parish_assignments)
  # remaining_assignments = sample(1:n_parishes, remaining_households, replace = TRUE)
  # parish_assignments = c(parish_assignments, remaining_assignments)
  # parish_assignments = sample(parish_assignments)
  
  # Now retrieve gamma for parish level consumption
  # We derive parish level consumption in the function where consumption is determined based on PIH, ROT, and KUJ. 
  set.seed(468)
  gamma_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  gamma_mean = summary(joneslist[[2]])$CoefTable[1,1]
  gamma_sd = summary(rotlist[[2]])$CoefTable[1,2]
  for (i in 1:n_households) {
    gamma_mat[,i] = rnorm(1, gamma_mean, gamma_sd ) 
  }
  
  # Retrieve the gamma for positive shocks to parish level consumption in the previous period. 
  set.seed(1000)
  gammapos_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  gammapos_mean = summary(joneslist[[2]])$CoefTable[2,1] + summary(joneslist[[2]])$CoefTable[1,1]
  gammapos_rho = joneslist[[2]]$vcov[1,2] 
  gammapos_sd = sqrt(summary(joneslist[[2]])$CoefTable[1,2]^2 + summary(joneslist[[2]])$CoefTable[2,2]^2 + 2 * gammapos_rho * summary(joneslist[[2]])$CoefTable[1,2] * summary(rotlist[[2]])$CoefTable[2,2])
  for (i in 1:n_households) {
    gammapos_mat[,i] = rnorm(1, gammapos_mean, gammapos_sd) 
  }
  
  
  # Finally, set up empty annual asset and consumption matrices 
  # The annual asset matrix will be used to determine asset dynamics 
  # Whereas the consumption matrix is necessary to initialise the first period consumption based on KUJ
  
  annual_asset_level = matrix(NA, nrow = n_years + 1, ncol = n_households)
  annual_asset_level[1,] = net_savings_mat[1,]
  
  closing_consumption_level = matrix(NA, nrow = n_years, ncol = n_households)
  
  #### Simulate the results. #####
  
  # First simulate the PIH with credit constraints. 
  # The simulation is a weekly timestep, with expected income re-determined annually. This assumes some stickiness in the sale of productive assets. 
  
  source("utilities\\PIH_credit_constraints.R")
  assets_PIH = PIH_credit_constraints(expected_income_mat,
                                      actual_income_mat,
                                      net_savings_mat,
                                      annual_asset_level,
                                      interest_rate,
                                      minimum_percapita_income)
  
  # Assess dynamics of assets_PIH
  source('utilities\\asset_dynamics.R')
  asset_dyanmics(assets_PIH, 
                yearlag = 5,
                xlim = c(-15,15),
                ylim = c(-15,15),
                title = "PIH with credit constraints",
                xtitle = "Log saving (lagged)",
                ytitle = "Log savings (current)")

  #With Rule of Thumb
  source("utilities\\ROT_simulation.R")
  savings_ROT_list = ROT_simulation(expected_income_mat,
                                    actual_income_mat,
                                    net_savings_mat,
                                    beta_mat,
                                    betapos_mat,
                                    annual_asset_level,
                                    interest_rate,
                                    minimum_percapita_income)
  
  asset_dyanmics(savings_ROT_list[[1]], 
                 yearlag = 5,
                 xlim = c(-15,15),
                 ylim = c(-15,15),
                 title = "ROT with symmetry",
                 xtitle = "Log saving (lagged)",
                 ytitle = "Log savings (current)")
  
  asset_dyanmics(savings_ROT_list[[2]], 
                 yearlag = 5,
                 xlim = c(-15,15),
                 ylim = c(-15,15),
                 title = "ROT with asymmetry",
                 xtitle = "Log saving (lagged)",
                 ytitle = "Log savings (current)")

  # Keeping up with the Joneses. 
  source("utilities\\KUJ_simulation.R")
  savings_KUJ_list = KUJ_simulation( expected_income_mat, 
                                     actual_income_mat,
                                     net_savings_mat,
                                     gamma_mat,
                                     gammapos_mat,
                                     annual_asset_level,
                                     interest_rate,
                                     minimum_percapita_income,
                                     closing_consumption_level)
  
  
  asset_dyanmics(savings_KUJ_list[[1]],
                yearlag = 5,
                xlim = c(-15,15),
                ylim = c(-15,15),
                title = "KUJ with symmetry",
                xtitle = "Log saving (lagged)",
                ytitle = "Log savings (current)")
  
  asset_dyanmics(savings_KUJ_list[[2]], 
                 yearlag = 5,
                 xlim = c(-15,15),
                 ylim = c(-15,15),
                 title = "KUJ with asymmetry",
                 xtitle = "Log saving (lagged)",
                 ytitle = "Log savings (current)")
  
}