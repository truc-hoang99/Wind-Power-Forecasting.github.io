# import data
test_data = read.csv('testing.csv') # only has environmental variables 
hist_data = read.csv("data.csv")
library("forecast")


###################################      PRELIMINARY ANALYSIS      ################################ 

a = 1/144
Time = seq(0,28.5,a)

# wind power vs. wind speed plot 
plot(hist_data$Speed_ms, hist_data$Power_scaled, xlab = "Wind Speed (m/s)", ylab = "Wind Power (scaled)",
     main = "Wind Speed vs Wind Power", col = 'blue', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)

# wind speed time series plot
plot(Time,hist_data$Speed_ms, ylab = "Wind Speed (m/s)", xlab = "Time (days)",
     main = "Wind Speed Over Time", type = 'l', col = 'orange',cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)



df_2= hist_data[,-1] # data frame without the time index
cor(df_2)

# correlation:
#              Temp (C)        Humidity   Solar_Irradiance  Power_scaled
#Speed_ms    0.274028779  -0.169861218     0.2765546        0.8885186


# (part 1 feedback): new data frame with non negative solar irradiance values 
new_df = c()
for (s in 1:dim(hist_data)[1]){
  new_df = hist_data[which(hist_data$Solar_Irradiance_Wm2 >= 0),]
}
cor(new_df$Speed_ms, new_df$Power_scaled) # 0.9047 

# The correlation between the wind speed and wind power during the day 
# strengthens the conclusion of the strong positive relationship with the increase from 0.889 to 0.905


###########################         MODELS &  METHODS        ###########################

###########################        Forecasting models         ############################

# develop forecasting model that forecast wind speed for 130 intervals ahead
# use sliding window to train and test models (time series data cannot be reshuffled)


nrolls = 30  # number of slides to be done 
hor = 130 # forecast horizon
rmse0 = rmse1 = rmse2 = rmse3 = rmse4 = vector(length = nrolls)
lm_er1 = lm_er2 = lm_er3 = lm_er4 = vector(length = nrolls)
pm_er1 = pm_er2 = pm_er3 = pm_er4 = vector(length = nrolls)

for (i in 1:30){
  
  nn = dim(hist_data)[1] - nrolls - hor + i
  hist_train = hist_data[1:nn,]
  hist_test = hist_data[(nn+1):(nn+hor),]
  
  # transform data into time series
  hist_train$Speed_ms = ts(hist_train$Speed_ms)
  hist_train$Temperature_C = ts(hist_train$Temperature_C)
  hist_train$Solar_Irradiance_Wm2 = ts(hist_train$Solar_Irradiance_Wm2)
  
  
  # Model Training
  f_train_arm = auto.arima(hist_train$Speed_ms)# ARIMA
  f_train_arf = arfima(hist_train$Speed_ms)# ARFIMA
  f_train_ses = ses(hist_train$Speed_ms, h = 130) # Simple Exponential Smoothing 
  f_train_arx = auto.arima(hist_train$Speed_ms,xreg=hist_train$Temperature_C+hist_train$Solar_Irradiance_Wm2) # ARIMAX (2 exogenous variables: temp and solar irradiance)
  
  # Forecast
  forecast_speed = forecast( f_train_arm,h = 130)
  forecast_speed2 = forecast(f_train_arf,h = 130)
  forecast_speed3 = forecast(f_train_ses, h = 130)
  forecast_speed4 = forecast(f_train_arx, xreg= hist_test$Temperature_C+hist_test$Solar_Irradiance_Wm2, h = 130)
  
  
  # Extract Point Forecast
  forecast_speed = forecast_speed$mean
  forecast_speed2 = forecast_speed2$mean
  forecast_speed3 = forecast_speed3$mean
  forecast_speed4 = forecast_speed4$mean
  
  # Regression model test (lm benchmark vs. best model from part 2: polynomial regression)
  df = data.frame(forecast_speed, hist_test[,3:6])
  df2 = data.frame(forecast_speed2, hist_test[,3:6])
  df3 = data.frame(forecast_speed3, hist_test[,3:6])
  df4 = data.frame(forecast_speed4, hist_test[,3:6])
  
  lmod = lm(Power_scaled ~ forecast_speed, data = df)
  lmod2 = lm(Power_scaled ~ forecast_speed2, data = df2)
  lmod3 = lm(Power_scaled ~ forecast_speed3, data = df3)
  lmod4 = lm(Power_scaled ~ forecast_speed4, data = df4) 
  
  pmod = lm(Power_scaled ~ Temperature_C + I(forecast_speed^2) + I(forecast_speed^3), data = df)
  pmod2 = lm(Power_scaled ~ Temperature_C + I(forecast_speed2^2) + I(forecast_speed2^3), data = df2)
  pmod3 = lm(Power_scaled ~ Temperature_C + I(forecast_speed3^2) + I(forecast_speed3^3), data = df3)
  pmod4 = lm(Power_scaled ~ Temperature_C + I(forecast_speed4^2) + I(forecast_speed4^3), data = df4)
  
  lm_pred1 = predict(lmod, df)
  lm_pred2 = predict(lmod2, df2)
  lm_pred3 = predict(lmod3, df3)
  lm_pred4 = predict(lmod4, df4)
  
  pm_pred1 = predict(pmod, df)
  pm_pred2 = predict(pmod2, df2)
  pm_pred3 = predict(pmod3, df3)
  pm_pred4 = predict(pmod4, df4)
  
  
  
  # forecast benchmark: persistence forecast 
  traindf_speed = hist_train$Speed_ms
  forecast_pers = rep(tail(traindf_speed,1),130)
  
  # RSME
  # wind speed forecast error 
  rmse0[i] = sqrt(mean((forecast_pers - hist_test$Speed_ms)^2))
  rmse1[i] = sqrt(mean((forecast_speed - hist_test$Speed_ms)^2))
  rmse2[i] = sqrt(mean((forecast_speed2 - hist_test$Speed_ms)^2))
  rmse3[i] = sqrt(mean((forecast_speed3 - hist_test$Speed_ms)^2))
  rmse4[i] = sqrt(mean((forecast_speed4 - hist_test$Speed_ms)^2))
  
  # wind power prediction error 
  lm_er1[i] = sqrt(mean((hist_test$Power_scaled - lm_pred1)^2))
  lm_er2[i] = sqrt(mean((hist_test$Power_scaled - lm_pred2)^2))
  lm_er3[i] = sqrt(mean((hist_test$Power_scaled - lm_pred3)^2))
  lm_er4[i] = sqrt(mean((hist_test$Power_scaled - lm_pred4)^2))
  
  
  pm_er1[i] = sqrt(mean((hist_test$Power_scaled - pm_pred1)^2))
  pm_er2[i] = sqrt(mean((hist_test$Power_scaled - pm_pred2)^2))
  pm_er3[i] = sqrt(mean((hist_test$Power_scaled - pm_pred3)^2))
  pm_er4[i] = sqrt(mean((hist_test$Power_scaled - pm_pred4)^2))
  
  
  
  cat(i, "\n")
}


mod_name = c("NAIVE", "ARIMA", "ARFIMA", "SES", "ARIMAX")
fs_error = round(c(mean(rmse0),mean(rmse1),mean(rmse2),mean(rmse3),mean(rmse4)), 7)
lm_error = round(c(0, mean(lm_er1),mean(lm_er2), mean(lm_er3), mean(lm_er4)),4)
pm_error = round(c(0, mean(pm_er1),mean(pm_er2), mean(pm_er3), mean(pm_er4)),4)

# final error metrics
for_er = cbind(mod_name, fs_error) #forecasting error 
reg_er = cbind(mod_name, lm_error, pm_error) #wind power prediction error b/t lm and polynomial mod
for_er
reg_er


# For every forecasting model used the polynomial regression model to predict power 
# performs better than the linear model relating wind speed to wind power (the regression benchmark)


#use best model to produce forecasts
fin.mod = auto.arima(hist_data$Speed_ms) #ARIMA
fin.mod2 = arfima(hist_data$Speed_ms) #ARFIMA
fin.mod3 = ses(hist_data$Speed_ms,h=130) #SES
fin.mod4 = auto.arima(hist_data$Speed_ms, xreg=hist_data$Temperature_C+hist_data$Solar_Irradiance_Wm2) #ARIMAX 

fin.pers = rep(tail(hist_data$Speed_ms,1),130)
fin.fcs = forecast(fin.mod,h = 130)
fin.fcs2 = forecast(fin.mod2,h = 130)
fin.fcs3 = forecast(fin.mod3,h = 130)
fin.fcs4 = forecast(fin.mod4,xreg=hist_data$Temperature_C+hist_data$Solar_Irradiance_Wm2, h = 130)

# visualization of final fit 
plot(seq(3000,4105,1),hist_data$Speed_ms[3000:4105], xlim=c(3000,4300), xlab="Time Index", ylab="Wind Speed", main="Forecast Method Comparison", type = "l") 
lines(seq(4106,4105+130,1),fin.pers,col="cyan",lwd=2)
lines(seq(4106,4105+130,1),fin.fcs$mean,col="blue",lwd=2)
lines(seq(4106,4105+130,1),fin.fcs2$mean,col="green",lwd=2)
lines(seq(4106,4105+130,1),fin.fcs3$mean,col="orange",lwd=2)
#lines(seq(4106,4105+130,1),fin.fcs4$mean,col="red",lwd=2)
abline(v=4105, col="purple", lty = 2)
legend("topleft",legend=c("Naive", "ARIMA", "ARFIMA", "SES"), 
       fill=c("cyan", "blue", "green", "orange"), cex=0.5)



# best model: ARFIMA due to the lowest error metric and visual analysis from the graph above. 
# final wind speed forecast 
new_windmod = arfima(hist_data$Speed_ms) 
wind_sp = forecast(new_windmod, h = 144)
wind_sp = wind_sp$mean 
test_data$Speed_ms = wind_sp # insert the wind speed forecasted to testing data set given





##################################             Regression Model                ##########################

# Based on the lowest rmse value, the team has chosen the ARFIMA model to forecast the wind speed
# The best regression model chosen in part 2 of the project outputted the second best predictions of class 

# forecast model: ARFIMA
# regression model: polynomial model (wind power)
# no need to reshuffle data

# model training 
lmod = lm(Power_scaled ~ Speed_ms, data = hist_data)
power_mod = lm(Power_scaled ~ Temperature_C + I(Speed_ms^2) + I(Speed_ms^3), data = hist_data)


power_pred = predict(power_mod,test_data)
final_ppred = data.frame(power_pred) #final wind power predictions

fin.fcs1 = forecast(fin.mod2,h = 144)
final_wind = fin.fcs1$mean
final_wpred = data.frame(final_wind) # final wind speed forecasts




############################       Final Predictions Visualization      ###########################
plot(hist_data$Speed_ms, hist_data$Power_scaled, xlab = "Wind Speed (m/s)", ylab = "Wind Power (scaled)",
     main = "Wind Speed vs Wind Power", col = 'blue')
points(final_wind,power_pred,lwd=2, col="orange", pch=19) # final predictions
legend("topleft",legend=c("Original Data","Final Predictions"),
       fill=c("blue","orange"), cex=0.5)


######################              CSV file export                 ########################
# The file should have separate columns for the forecast wind speed and predicted wind power. 
final_pred = write.csv(c(final_wpred,final_ppred),"C:/Users/Christine Domercant/Documents/Spring 2023/Industrial Informatics\\DataBosses_finalpred1.csv") #_finalpred is a csv file from part 2 


