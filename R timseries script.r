mydata <- read.csv("isp_data.csv") # read data into data frame
results=lm(MB ~ t+tsq+Day.of.Week_1+Day.of.Week_2+Day.of.Week_3+Day.of.Week_4+Day.of.Week_5+Day.of.Week_6, data=mydata) # multiple linear regression with t,tsq,dummy variables
residue_results<-results$residuals   # extract residuals from results
acf(residue_results)    # calculate acf plot
c(acf(residue_results)$lag,acf(residue_results)$acf)  # see lags and acf values
ar_results<-Arima(residue_results,order=c(15,0,2))  # develop arima model
ar_residuals<-ar_results$residuals # extract residuals values
mydata2 <- read.csv("isp_data2.csv") # get input data with lags
results2=lm(MB~t+tsq+Day.of.Week_1+Day.of.Week_2+Day.of.Week_3+Day.of.Week_4+Day.of.Week_5+Day.of.Week_6+Lag1+Lag2+Lag3+Lag4+Lag5+Lag6+Lag7+Lag8+Lag9+Lag10+Lag11+Lag12+Lag13+Lag14+Lag15, data=mydata2)  # multiple linear regression with t,tsq,dummy variables AND LAGS
results2_fitted<-results2$fitted.values  # extract fitted values from results2
results_sum<-results2_fitted + ar_residuals # add fitted and error values
plot(mydata$MB,col="red") # plot original graph
lines(results_sum,col="blue") # plot new predicted values