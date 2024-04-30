# This script runs the wealth dynamics simulations. 
# The first part of the script retrieves the parameters and initial conditions 
# Then it calls individual functions that perform the simulations
# And then returns plots of lagged and current savings against the 45 degree line.

runSimulations = function(bg_data, data, rotlist, joneslist, RESID_INC) {
  #Parameters
  n_households = 1000
  n_weeks = 52
  n_years = 25 # Number of years we observe the behaviour. 
  
  # INCOME is simulated as the sum of a deterministic income and a stochastic unexpected income draw. 
  # The definition of the deterministic income is a minimum per capita income + return on per capita assets. 
  # First we will determine an appropriate definition of minimum per capita income and an interest rate
  # This is done to approximately match the distribution of average per capita income per week. 
  # This occurs at an interest rate for assets (net debts) of 10%, 
  # And a minimum per capita income of 7000 UGX per week. 
  # Below we confirm this with the histograms. 
  
  interest_rate = 0.25
  minimum_percapita_income = 8000
  
  total_assets_percapita = rowSums(cbind(bg_data$VAL_ASSETS_BICYCLE,
                                         bg_data$VAL_ASSETS_BODA,
                                         bg_data$VAL_ASSETS_SAVINGS,
                                         bg_data$VAL_ASSETS_SMARTPHONE,
                                         bg_data$VAL_ASSETS_TV),
                                         na.rm = TRUE)/bg_data$NUM_IN_HOUSEHOLD
  
  #Trim outliers
  tails = quantile(total_assets_percapita, probs=c(0.05, .95), na.rm = TRUE)
  
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
  
  
  # Actual income is is defined by this minimum per capita income and return on asset, plus the random shock. 
  # This requires a distribution of per capita assets. 
  
  #Remove large outliers for the savings and determine aggregate savings per capita.
  net_savings_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  mean_net_starting_assets = mean(subset(total_assets_percapita, total_assets_percapita > tails[1] & total_assets_percapita < tails[2]))
  sd_net_starting_assets = sd(subset(total_assets_percapita, total_assets_percapita > tails[1] & total_assets_percapita < tails[2]))
  for (i in 1:n_households) {
    net_savings_mat[1,i] = rnorm(1, mean_net_starting_assets, sd_net_starting_assets) 
  } 
  net_savings_mat[net_savings_mat<tails[1]] = tails[1]
  
  deterministic_income_mat = minimum_percapita_income + (net_savings_mat[1,] * interest_rate)/n_weeks
  #Note this updates in the simulations based on asset levels, but we initialise the size of the matrix here. 
  deterministic_income_mat = do.call(rbind, replicate(n_weeks, deterministic_income_mat , simplify=FALSE)) 
  
  # Have a stochastically determined random shock drawn from a mean and variance of change in unexpected income from the results. This is unexpected income
  # We assume variation occurs around the income drawn from assets. THe minimum consumption requirements are assumed risk free and expected.  
  # This implies a heteroskedastic variation at higher asset levels. 
  # We draw the sd from the residuals predicting the expected income. 
  # This is assumed to be the SD around the mean asset level income. 
  # We then scale the SD based on the individuals asset level here and in the simulations. 
  # We based this on the s.e of RESID_INC being approximate to the s.e. of EXPECT_INC in the modelling. 
  
  unexpected_income_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  sd_unexpected_inc = 1000*exp(sd(RESID_INC, na.rm = TRUE)) - 1000
  for (i in 1:n_households) {
    saving_proportion = net_savings_mat[1,i]/mean(net_savings_mat[1,])
    if(saving_proportion <0){
      saving_proportion = 0
    } else if (saving_proportion > 1){
      saving_proportion = 1
    } 
    unexpected_income_mat[,i] = rnorm(n_weeks, 0, saving_proportion * sd_unexpected_inc)
  }
  
  # Sum this with mean income to give actual income. If negative, code as 0
  actual_income_mat = deterministic_income_mat +  unexpected_income_mat
  actual_income_mat[actual_income_mat<0] = 0
  
  # Draw from the results alpha, from a distribution with mean equal to the coefficient and sd equal to the standard error.
  # alpha represents the responsiveness of consumption to changes in expected income.
  # truncate at zero
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
  beta_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  beta_mean = summary(rotlist[[2]])$CoefTable[3,1]
  beta_sd = summary(rotlist[[2]])$CoefTable[3,2]
  for (i in 1:n_households) {
    beta_mat[,i] = rnorm(1, beta_mean, beta_sd ) 
  }
  
  # Draw from the results beta for positive unexpected shocks, from a distribution with mean equal to the coefficient and sd equal to the standard error.
  # beta represents the responsiveness of consumption to changes in positive unexpected income under RULE OF THUMB. 
  betapos_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  betapos_mean = summary(rotlist[[2]])$CoefTable[3,1]+ summary(rotlist[[2]])$CoefTable[4,1]
  betapos_rho = rotlist[[2]]$vcov[4,3] 
  betapos_sd = sqrt(summary(rotlist[[2]])$CoefTable[3,2]^2 + summary(rotlist[[2]])$CoefTable[4,2]^2 + 2 * betapos_rho * summary(rotlist[[2]])$CoefTable[3,2] * summary(rotlist[[2]])$CoefTable[4,2])
  for (i in 1:n_households) {
    betapos_mat[,i] = rnorm(1, betapos_mean, betapos_sd ) 
  }

  
  # Now retrieve gamma for parish level consumption
  # We derive parish level consumption in the function where consumption is determined based on PIH, ROT, and KUJ. 
  # We assume all households belong to the same parish - as households are randomly drawn from the same distribution
  # The mean of the entire sample will approach the mean of any assumed subsamples from this distribution anyway. 
  gamma_mat = matrix(NA, nrow = n_weeks, ncol = n_households)
  gamma_mean = summary(joneslist[[2]])$CoefTable[1,1]
  gamma_sd = summary(rotlist[[2]])$CoefTable[1,2]
  for (i in 1:n_households) {
    gamma_mat[,i] = rnorm(1, gamma_mean, gamma_sd ) 
  }
  
  # Retrieve the gamma for positive shocks to parish level consumption in the previous period. 
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
  assets_PIH = PIH_credit_constraints(deterministic_income_mat,
                                      actual_income_mat,
                                      net_savings_mat,
                                      annual_asset_level,
                                      interest_rate,
                                      minimum_percapita_income,
                                      sd_unexpected_inc, 
                                      tails[1])
  
  # Assess dynamics of assets_PIH
  source('utilities\\asset_dynamics.R')
  asset_dyanmics(assets_PIH, 
                yearlag = 3,
                xlim = c(-1, 8),
                ylim = c(-1, 8),
                title = "PIH with credit constraints",
                xtitle = "Log saving (lagged t-3)",
                ytitle = "Log savings (current)")

  #With Rule of Thumb
  source("utilities\\ROT_simulation.R")
  savings_ROT_list = ROT_simulation(deterministic_income_mat,
                                    actual_income_mat,
                                    net_savings_mat,
                                    beta_mat,
                                    betapos_mat,
                                    annual_asset_level,
                                    interest_rate,
                                    minimum_percapita_income,
                                    sd_unexpected_inc,
                                    tails[1])
  
  asset_dyanmics(savings_ROT_list[[1]], 
                 yearlag = 3,
                 xlim = c(-1, 8),
                 ylim = c(-1, 8),
                 title = "ROT with symmetry",
                 xtitle = "Log saving (lagged t-3)",
                 ytitle = "Log savings (current)")
  
  asset_dyanmics(savings_ROT_list[[2]], 
                 yearlag = 3,
                 xlim = c(-1, 8),
                 ylim = c(-1, 8),
                 title = "ROT with asymmetry",
                 xtitle = "Log saving (lagged t-3)",
                 ytitle = "Log savings (current)")

  # Keeping up with the Joneses. 
  source("utilities\\KUJ_simulation.R")
  savings_KUJ_list = KUJ_simulation( deterministic_income_mat, 
                                     actual_income_mat,
                                     net_savings_mat,
                                     gamma_mat,
                                     gammapos_mat,
                                     annual_asset_level,
                                     interest_rate,
                                     minimum_percapita_income,
                                     closing_consumption_level,
                                     sd_unexpected_inc,
                                     tails[1])
  
  
  asset_dyanmics(savings_KUJ_list[[1]],
                yearlag = 3,
                xlim =  c(-1, 8),
                ylim =  c(-1, 8),
                title = "KUJ with symmetry",
                xtitle = "Log saving (lagged)",
                ytitle = "Log savings (current)")
  
  asset_dyanmics(savings_KUJ_list[[2]], 
                 yearlag = 3,
                 xlim = c(-1, 8), 
                 ylim = c(-1, 8),
                 title = "KUJ with asymmetry",
                 xtitle = "Log saving (lagged t-3)",
                 ytitle = "Log savings (current)")
  
  # ALL
  source("utilities\\All_simulation.R")
  savings_All_list = All_simulation( deterministic_income_mat, 
                                     actual_income_mat,
                                     net_savings_mat,
                                     beta_mat,
                                     betapos_mat,
                                     gamma_mat,
                                     gammapos_mat,
                                     annual_asset_level,
                                     interest_rate,
                                     minimum_percapita_income,
                                     closing_consumption_level,
                                     sd_unexpected_inc,
                                     tails[1])
  
  
  asset_dyanmics(savings_All_list[[1]],
                 yearlag = 3,
                 xlim =  c(-1, 8),
                 ylim =  c(-1, 8),
                 title = "ROT and KUJ with symmetry",
                 xtitle = "Log saving (lagged t-3)",
                 ytitle = "Log savings (current)")
  
  asset_dyanmics(savings_All_list[[2]], 
                 yearlag = 3,
                 xlim = c(min(savings_All_list[[1]]), 8),
                 ylim = c(min(savings_All_list[[1]]), 8),
                 title = "ROT and KUJ with asymmetry",
                 xtitle = "Log saving (lagged t-3)",
                 ytitle = "Log savings (current)")
  
  
  
}