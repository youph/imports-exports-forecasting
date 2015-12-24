# Using the quarterly ABS data on imports and exports, construct a ranked list of countries exporting from and 
# importing to Australia, in terms of weight, and value.
library(forecast)
# Import the quarterly exports data (international export freight only, does not include coastal freight):
exports_quarterly_raw <- read.csv("~/Projects/Shipping/Data/Imports & Exports timeseries/ABS quarterly/exports-quarterly.csv", colClasses = "character")
# sort on the year:
exports_quarterly_raw <- exports_quarterly_raw[order(as.Date(exports_quarterly_raw$YEAR, format="%Y")),]

# Import the quarterly imports data (international import freight only, does not include coastal freight):
imports_quarterly_raw <- read.csv("~/Projects/Shipping/Data/Imports & Exports timeseries/ABS quarterly/imports-quarterly.csv", colClasses = "character")
# sort on the year:
imports_quarterly_raw <- imports_quarterly_raw[order(as.Date(imports_quarterly_raw$Year, format="%Y")),]
########################################################################################################################
# Compare the data with BITRE data for total international sea freight (exports and imports):
# Total weight exported and imported by sea via all AU ports, yearly (financial year is 1 July-30 June):
period <- 2003:2012
financial_years <- character()
for (year in period){
  financial_years <- append(financial_years, paste(year,'-',year+1, sep=''))
}
sea_freight_weight_financial_years <- data.frame("Financial Year"=financial_years, "exports_ALL_PORTS_ABS"=NA, "exports_ALL_PORTS_BITRE"=NA,
                                                 "imports_ALL_PORTS_ABS"=NA, "imports_ALL_PORTS_BITRE"=NA)
# Australia's international sea freight (total), from BITRE (asf_2012_13.pdf, Table 1.6, p.25)
sea_freight_weight_financial_years$exports_ALL_PORTS_BITRE <- c(558.3, 610.6, 626.4, 657.1, 706.9, 753.2, 861.9, 881.3, 973.2, 1070.2)  # in Mtons
sea_freight_weight_financial_years$imports_ALL_PORTS_BITRE <- c(64.2, 69.9, 72.7, 77.8, 84.6, 81.6, 85.7, 92.1, 94.9, 99.0)             # in Mtons
for (row in 1:nrow(sea_freight_weight_financial_years)){
  year <- as.integer(strsplit(as.character(sea_freight_weight_financial_years[row,]$Financial.Year), '-')[[1]][1])
  sea_freight_weight_financial_years[row,]$exports_ALL_PORTS_ABS <- sum(as.numeric(exports_quarterly_raw[exports_quarterly_raw$Mode_of_transport=="SEA" 
                                                                                                     & ((exports_quarterly_raw$YEAR==year & (exports_quarterly_raw$Qtr==3 | exports_quarterly_raw$Qtr==4)) | 
                                                                                                          (exports_quarterly_raw$YEAR==year+1 & (exports_quarterly_raw$Qtr==1 | exports_quarterly_raw$Qtr==2))),]$Weight), na.rm = TRUE)/1e6    
  sea_freight_weight_financial_years[row,]$imports_ALL_PORTS_ABS <- sum(as.numeric(imports_quarterly_raw[imports_quarterly_raw$Mode_of_transport=="SEA" 
                                                                                                     & ((imports_quarterly_raw$Year==year & (imports_quarterly_raw$Qtr==3 | imports_quarterly_raw$Qtr==4)) | 
                                                                                                          (imports_quarterly_raw$Year==year+1 & (imports_quarterly_raw$Qtr==1 | imports_quarterly_raw$Qtr==2))),]$Weight), na.rm = TRUE)/1e6    
  cat("Financial year",as.character(sea_freight_weight_financial_years[row,]$Financial.Year),": Total export", 
      sea_freight_weight_financial_years[row,]$exports_ALL_PORTS_ABS,"Mtons (ABS)",
      "[",sea_freight_weight_financial_years[row,]$exports_ALL_PORTS_BITRE,"Mtons (BITRE) ]. ")
  cat("Total import",sea_freight_weight_financial_years[row,]$imports_ALL_PORTS_ABS,"Mtons (ABS)",
      "[",sea_freight_weight_financial_years[row,]$imports_ALL_PORTS_BITRE,"Mtons (BITRE) ].\n")
}
# Conclusion: the totals for exports are approximately the same.
########################################################################################################################
# Exclude reimports and reexports via excluding codes 98 and 99, as well as exports_quarterly$State_origin=="REX" and imports_quarterly$Country=="Australia (Re-imports)":
exports_quarterly <- exports_quarterly_raw[exports_quarterly_raw$code2 != 98 & exports_quarterly_raw$code2 != 99 &
                                             exports_quarterly_raw$State_origin != "REX",]

imports_quarterly <- imports_quarterly_raw[imports_quarterly_raw$Code2 != 98 & imports_quarterly_raw$Code2 != 99 &
                                             imports_quarterly_raw$Country != "Australia (Re-imports)",]

years = unique(exports_quarterly$YEAR)
years <- years[order(as.Date(years, format="%Y"))]
########################################################################################################################
# Compare the data with BITRE data for total international sea freight (exports and imports):
period <- as.integer(years[-length(years)])  # all but the last year
financial_years <- character()
for (year in period){
  financial_years <- append(financial_years, paste(year,'-',year+1, sep=''))
}
# Total weight exported by sea via all AU ports, yearly:
exports_sea_weight_yearly <- data.frame("Financial Year"=financial_years, "ALL_PORTS"=NA)
for (port in sort(unique(exports_quarterly$POrt_of_loading))){
  exports_sea_weight_yearly[,port] <- NA 
}

for (row in 1:nrow(exports_sea_weight_yearly)){
  year <- as.integer(strsplit(as.character(exports_sea_weight_yearly[row,]$Financial.Year), '-')[[1]][1])
  exports_sea_weight_yearly[row,'ALL_PORTS'] <- sum(as.numeric(exports_quarterly[exports_quarterly$Mode_of_transport=="SEA" & 
                                                                                   ((exports_quarterly$YEAR==year & (exports_quarterly$Qtr==3 | exports_quarterly$Qtr==4)) |
                                                                                      (exports_quarterly$YEAR==year+1 & (exports_quarterly$Qtr==1 | exports_quarterly$Qtr==2))),]$Weight))/1e6
  cat('\nFinancial year',as.character(exports_sea_weight_yearly[row,]$Financial.Year),': total weight exported by "SEA" via all AU ports:',exports_sea_weight_yearly[row,'ALL_PORTS'],'Mtons \n')
  for (port in sort(unique(exports_quarterly$POrt_of_loading))){
    exports_sea_weight_yearly[row,port] <- sum(as.numeric(exports_quarterly[exports_quarterly$Mode_of_transport=="SEA" & exports_quarterly$POrt_of_loading==port & 
                                                                              ((exports_quarterly$YEAR==year & (exports_quarterly$Qtr==3 | exports_quarterly$Qtr==4)) |
                                                                                 (exports_quarterly$YEAR==year+1 & (exports_quarterly$Qtr==1 | exports_quarterly$Qtr==2))),]$Weight))/1e6
    cat('Financial year',as.character(exports_sea_weight_yearly[row,]$Financial.Year),': total weight exported by "SEA" via',port,':',exports_sea_weight_yearly[row,port],'Mtons \n')
  }
}
# Compare exports and imports via Port Kembla, with BITRE data:
# Australia's international sea freight via top-10 ports, from BITRE: asf_2012_13.pdf, Table 1.4, p.22

# Total weight imported by sea via all AU ports, yearly:
imports_sea_weight_yearly <- data.frame("Financial Year"=financial_years, "ALL_PORTS"=NA)
for (port in sort(unique(imports_quarterly$Port_of_Discharge))){
  imports_sea_weight_yearly[,port] <- NA 
}

for (row in 1:nrow(imports_sea_weight_yearly)){
  year <- as.integer(strsplit(as.character(imports_sea_weight_yearly[row,]$Financial.Year), '-')[[1]][1])
  imports_sea_weight_yearly[row,'ALL_PORTS'] <- sum(as.numeric(imports_quarterly[imports_quarterly$Mode_of_transport=="SEA" & 
                                                                                   ((imports_quarterly$Year==year & (imports_quarterly$Qtr==3 | imports_quarterly$Qtr==4)) |
                                                                                      (imports_quarterly$Year==year+1 & (imports_quarterly$Qtr==1 | imports_quarterly$Qtr==2))),]$Weight))/1e6
  cat('\nFinancial year',as.character(imports_sea_weight_yearly[row,]$Financial.Year),': total weight exported by "SEA" via all AU ports:',imports_sea_weight_yearly[row,'ALL_PORTS'],'Mtons \n')
  for (port in sort(unique(imports_quarterly$Port_of_Discharge))){
    imports_sea_weight_yearly[row,port] <- sum(as.numeric(imports_quarterly[imports_quarterly$Mode_of_transport=="SEA" & imports_quarterly$Port_of_Discharge==port & 
                                                                              ((imports_quarterly$Year==year & (imports_quarterly$Qtr==3 | imports_quarterly$Qtr==4)) |
                                                                                 (imports_quarterly$Year==year+1 & (imports_quarterly$Qtr==1 | imports_quarterly$Qtr==2))),]$Weight))/1e6
    cat('Financial year',as.character(imports_sea_weight_yearly[row,]$Financial.Year),': total weight imported by "SEA" via',port,':',imports_sea_weight_yearly[row,port],'Mtons \n')
  }
}

# -----------------------------------------------------------------------------------------------------------------------
# Define a dataframe of exporters, to determine top exporters in each year and overall:
exporting_countries = sort(unique(exports_quarterly$Country))
exporting_years = unique(exports_quarterly$YEAR)
exporting_years <- exporting_years[order(as.Date(exporting_years, format="%Y"))]
ports <- "ALL_PORTS"
exporters <- expand.grid(exporting_countries ,exporting_years, ports)
colnames(exporters) <- c("Country", "Year", "Port")
exporters <- cbind(exporters, "Tot_Value_FOB_AUD"=NA, "Tot_Weight"=NA)

# Fill-in 'exporters':
for (row in 1:nrow(exporters)){
  country <- exporters[row,'Country']
  year <- exporters[row,'Year']
  port <- exporters[row,'Port']
  if (port=="ALL_PORTS"){
    subset <- exports_quarterly[which(exports_quarterly$Country==country & exports_quarterly$YEAR==year & exports_quarterly$Mode_of_transport=="SEA"),]
  } 
  else {
    subset <- exports_quarterly[which(exports_quarterly$Country==country & exports_quarterly$YEAR==year & exports_quarterly$POrt_of_loading==port & exports_quarterly$Mode_of_transport=="SEA"),]
  }
  tot_value <- sum(as.numeric(subset$Value_FOB_AUS), na.rm = TRUE)/1e6
  tot_weight <- sum(as.numeric(subset$Weight), na.rm = TRUE)/1e6
  exporters[row,'Tot_Value_FOB_AUD'] <- tot_value
  exporters[row,'Tot_Weight'] <- tot_weight
  cat('row',row,'of',nrow(exporters),': year',as.character(year),', country',as.character(country),', via',as.character(port),': total value exported =',tot_value,'Million AUD, total weight =',tot_weight*1e6,'tons\n')
}

# --------------------------------------------------------------------------------------------------------------------------------------------

# Plot the yearly exports by weight:
coef = 1e2
plot(as.integer(as.factor(exports_sea_weight_yearly$Financial.Year)), exports_sea_weight_yearly$ALL_PORTS/coef, type="b", pch=19, col='black', 
     ylim=c(min(exports_sea_weight_yearly[,'Port Botany']), max(exports_sea_weight_yearly[,-1])/coef), xlab="Year", ylab="Yearly exports, tons")

points(as.integer(as.factor(exports_sea_weight_yearly$Financial.Year)), exports_sea_weight_yearly$Sydney, type="b", pch=19, col='green')
points(as.integer(as.factor(exports_sea_weight_yearly$Financial.Year)), exports_sea_weight_yearly$`Conf Aust Ports`/coef*10, type="b", pch=19, col='red')
points(as.integer(as.factor(exports_sea_weight_yearly$Financial.Year)), exports_sea_weight_yearly$`NOT AVAILABLE`, type="b", pch=19, col='cyan')
points(as.integer(as.factor(exports_sea_weight_yearly$Financial.Year)), exports_sea_weight_yearly$Kurnell, type="b", pch=19, col='orange')
lines(as.integer(as.factor(exports_sea_weight_yearly$Financial.Year)), exports_sea_weight_yearly$`Port Botany`, type="b", lwd=3, pch=19, col='blue')
lines(as.integer(as.factor(exports_sea_weight_yearly$Financial.Year)), exports_sea_weight_yearly$`Port Botany` + exports_sea_weight_yearly$Sydney 
      + exports_sea_weight_yearly$Kurnell, type="b", lwd=3, pch=19, col='gray')
legend('topleft',legend=c("ALL PORTS/100", "Port Botany", "Sydney", "Kurnell", "Conf Austr Ports/10", "NOT AVAILABLE", "All Sydney ports"), pch=c(19), col=c('black', 'blue','green','orange','red','cyan','gray'))

# Check whether the sum of exports from all Australian ports, and from all Sydney ports in this dataset (Sydney, Port Botany, Kurnell, Other NSW ports) match the corresponding exports according to BITRE data:
# total exports from all AU ports in 2000(quaters 3, 4) - 2001 (quaters 1,2):
year1 <- 2006
year2 <- year1+1
tot_exports_year1_year2 <- sum(as.numeric(exports_quarterly[exports_quarterly$Mode_of_transport=="SEA" & 
                                                            ((exports_quarterly$YEAR==year1 & (exports_quarterly$Qtr==3 | exports_quarterly$Qtr==4)) 
                                                          | (exports_quarterly$YEAR==year2 & (exports_quarterly$Qtr==1 | exports_quarterly$Qtr==2))),'Weight']))
tot_exports_year1_year2_TRUE_PA <- 626519064
tot_exports_year1_year2_TRUE_BITRE <- 716.1e6
(tot_exports_year1_year2/tot_exports_year1_year2_TRUE_PA - 1)*100
(tot_exports_year1_year2/tot_exports_year1_year2_TRUE_BITRE - 1)*100


# *****************************************************-------------------------------------------------------------------
# Time series analysis:
# Quarterly weights by port of loading:
ts_start <- c(1995,1)
ts_end <- c(2014,2)
tmp <- ts(NA, frequency=4, start=ts_start, end=ts_end)
exports_weight_quarterly_by_port <- ts(matrix(NA,nrow = length(tmp), ncol=length(sort(unique(exports_quarterly$POrt_of_loading)))+1), frequency=4, start = ts_start, 
                               end = ts_end, names = c('ALL_PORTS',sort(unique(exports_quarterly$POrt_of_loading))))
for (r in 1:nrow(exports_weight_quarterly_by_port)){
  year <- ts_start[1] + as.integer((r-1)/4)
  quarter <- (r-1) %% 4 + 1      # quarter
  exports_weight_quarterly_by_port[r,'ALL_PORTS'] <- sum(as.numeric(exports_quarterly[exports_quarterly$Mode_of_transport=="SEA" & exports_quarterly$YEAR==year & exports_quarterly$Qtr==quarter,]$Weight))/1e6
  cat('Year',year,', Qtr',quarter,': total weight exported by "SEA" via all AU ports:',exports_weight_quarterly_by_port[r,'ALL_PORTS'],'Mtons \n')
  
  for (port in sort(unique(exports_quarterly$POrt_of_loading))){
    exports_weight_quarterly_by_port[r,port] <- sum(as.numeric(exports_quarterly[exports_quarterly$Mode_of_transport=="SEA" & 
                                                                                   exports_quarterly$POrt_of_loading==port & exports_quarterly$YEAR==year & 
                                                                                   exports_quarterly$Qtr==quarter,]$Weight))/1e6
    cat('Year',year,', Qtr',quarter,': total weight exported by "SEA" via',port,':',exports_weight_quarterly_by_port[r,port],'Mtons \n')
  }
}

sydney_ports <- c('Port Botany', 'Sydney', 'Kurnell')
# add a new ts to exports_weight_quarterly_by_port:
names <- colnames(exports_weight_quarterly_by_port)   # save the col names
exports_weight_quarterly_by_port <- cbind(exports_weight_quarterly_by_port, 'all_sydney_ports'=rowSums(exports_weight_quarterly_by_port[,sydney_ports]))
# restore the col names of the modified ts:
colnames(exports_weight_quarterly_by_port) <- c(names, 'all_sydney_ports')      # sub("^.*\\.", "", colnames(exports_weight_quarterly_by_port)) 

# Plot the quarterly exports by weight, for selected ports:
ports <- c('Port Botany', 'Sydney', 'Kurnell', 'all_sydney_ports')
plot.ts(exports_weight_quarterly_by_port[,ports], plot.type = 'single', col=rainbow(length(ports)), xlab="Year", ylab="Quarterly exports, Mtons")
legend('topleft', legend=ports, col=rainbow(length(ports)), lty=1)

# Plot yearly exports for the selected ports:
plot.ts(aggregate(exports_weight_quarterly_by_port[,ports], nfrequency = 1), plot.type = 'single', col=rainbow(length(ports)), xlab="Year", ylab="Yearly exports, Mtons")
legend('topleft', legend=ports, col=rainbow(length(ports)), lty=1)

exports_all_ports <- (exports_weight_quarterly_by_port[,'ALL_PORTS'])   # exports from all ports, quarterly, in Mtons
exports_all_ports_forecast <- HoltWinters(exports_all_ports)            # forecast over the same time range as the actual data
exports_all_ports_forecast_future <- forecast.HoltWinters(exports_all_ports_forecast, h=20)   # h quarters ahead from the last observation
plot(exports_all_ports_forecast_future, main='Port throughput forecast', xlab = 'Year', ylab='Quarterly exports from all AU ports, Mtons')
# Look at the correlogram of in-sample forecast errors for lags 1-20, to figure out whether the forecast errors for successive predictions are correlated or not.
acf(exports_all_ports_forecast_future$residuals, lag.max = 20)
# Test whether there is a significant evidence for non-zero correlations at lags 1-20, using the Ljung-Box test:
Box.test(exports_all_ports_forecast_future$residuals, lag=20, type="Ljung-Box")
# Here the Ljung-Box test statistic is 22.7, and the p-value is 0.3, so there is little evidence of non-zero autocorrelations in the in-sample forecast 
# errors at lags 1-20.

# To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast errors are normally distributed 
# with mean zero and constant variance. To check whether the forecast errors have constant variance, we can make a time plot of the in-sample forecast errors:
plot.ts(exports_all_ports_forecast_future$residuals)
# The variance of the forecast errors has increased after ~2008, which perhaps is due to more turbulent trade after the GFC.

# To check whether the forecast errors are normally distributed with mean zero (ideal case), plot a histogram of the forecast errors, 
# with an overlaid normal curve that has mean zero and the same standard deviation as the distribution of forecast errors.
# To do this, define an R function 'plotForecastErrors()':
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/2  # bin size is 1/2 of the interquantile range of forecasterrors
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*3
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(100000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(exports_all_ports_forecast_future$residuals)
# The distribution of forecast errors seems to be approximately normally distributed with zero mean. 
# Combined with the fact that the Ljung-Box test showed that there is no evidence of non-zero autocorrelations in the in-sample forecast errors, 
# this suggests that the predictive model based on the 3-step exponential smoothing (Holt-Winters forecast) is adequate, and probably cannot be further improved.
# Furthermore, the assumptions that the 80% and 95% predictions intervals were based upon (that there are no autocorrelations in the forecast errors, 
# and the forecast errors are normally distributed with mean zero and constant variance) are probably valid.

# However, we'll try to improve the model anyway (e.g., by including exogenous time series in the model via dynamic factors), and will compare whatever models 
# we come up with with this 'baseline' model.