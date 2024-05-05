# Asset dynamics script
# This script estimates a non-parametric function for the change in asset levels between periods. 
# The asset levels are defined by the simulation models turning on and off different behavioural parameters e.g. ROT.

asset_dyanmics = function(annual_asset_level, 
                          yearlag = 1, 
                          xlim = c(-40,40),
                          ylim = c(-40,40),
                          title = "Savings dynamics", 
                          xtitle = "Lagged", 
                          ytitle = "Current"){
  
  #Manipulate the dataframe to be two columns - CURRENT AND LAGGED
  n_weeks = nrow(annual_asset_level)
  
  current = annual_asset_level[,1]
  week_vec = 1:n_weeks
  for (c in 2:ncol(annual_asset_level)){
    current = c(current, annual_asset_level[,c])
    week_vec = c(week_vec, 1:n_weeks)
  }
  
  lagged=rep(NA,length(current))
  for(i in 1:length(current)){
    if (week_vec[i] < yearlag+1){
      lagged[i] = NA
    } else {
      lagged[i]=current[i-yearlag]
    } 
  }
  
  #Structure into dataframe and remove rows with lagged value
  polydat = data.frame(CURRENT = current,
                       LAGGED = lagged)
  polydat = na.omit(polydat)
  
  if (title !="PIH with credit constraints"){
    polydat = subset(polydat, polydat$CURRENT != polydat$LAGGED)
  }
  
  p = ggplot(data = polydat, aes(x = LAGGED, y = CURRENT)) +
    geom_smooth(method = "loess", color = "red", linetype = "dashed") +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()+
    xlim(xlim) +
    ylim(ylim) +
    xlab(xtitle) +
    ylab(ytitle) +
    ggtitle(title) + 
    theme(
      axis.text = element_text(size = 14),  
      axis.title = element_text(size = 14),  
      plot.title = element_text(size = 16)  
    ) 

  print(p)
  ggsave(filename = paste("Simulation_", title, ".jpg", sep = ""), p)

}