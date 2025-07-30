# Team Data Bosses
# Anne Christine, Tina Truc, Tasnim

# Task 2: Data Visualization and Preliminary Analysis

# i)
data = read.csv("data.csv")

plot(data$Speed_ms, data$Power_scaled, xlab = "Wind Speed (m/s)", ylab = "Wind Power (scaled)",
     main = "Wind Speed vs Wind Power", col = 'blue', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)

# When examining the plot, it appears that as wind speed increases, wind power
# also increases. There is a higher concentration of data points at lower wind speeds
# and wind power.

# When examining the plot, we estimate that the wind speed cut-off is approximately
# 0.5 m/s.

# ii)

a = 1/144

Time = seq(0,28.5,a)

plot(Time,data$Speed_ms, ylab = "Wind Speed (m/s)", xlab = "Time (days)",
     main = "Wind Speed Over Time", type = 'l', col = 'orange',cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)

# A majority of the wind speeds over time are less than 2.5 m/s. Around day 5,
# we see the maximum wind speed recorded at 5.757 m/s.

# iii)

plot(Time, data$Power_scaled, ylab = "Wind Power (scaled)", xlab = "Time (days)", main =
       "Wind Power versus Time", col = 'purple',type = 'l',cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)

# A majority of wind power values are from 0.0 to 0.2. The power does not fluctuate
# as much as speed over the period of time.

## iv)

par(mfrow = c(2,1))

plot(Time,data$Speed_ms, ylab = "Wind Speed (m/s)", xlab = "Time (days)",
     main = "Wind Speed Over Time", type = 'l', col = 'orange', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(Time, data$Power_scaled, ylab = "Wind Power (scaled)", xlab = "Time (days)", main =
       "Wind Power versus Time", type = 'l', col = 'purple', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)

# When examining both the wind speed and wind power over time, they both seem
# to follow a similar pattern. However, it is easier to see changes in wind speed
# than wind power.

## v)

par(mfrow = c(2,2))

plot(data$Speed_ms,Time, xlab = "Wind Speed (m/s)", ylab = "Time (days)",
     main = "Wind Speed Over Time", col = 'orange', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Speed_ms, data$Temperature_C, xlab = "Wind Speed (m/s)", ylab = "Temperature (deg C)",
     main = "Wind Speed vs Temperature", col = 'red', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Speed_ms, data$Humidity, xlab = "Wind Speed (m/s)", ylab = "Humidity",
     main = "Wind Speed vs Humidity (g.m^-3)", col = 'pink', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Speed_ms, data$Solar_Irradiance_Wm2, xlab = "Wind Speed (m/s)", ylab = "Solar Irradiance (W/m^2)",
     main = "Wind Speed vs Solar Irradiance", col = 'brown', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)

# The plots seen previously show a relationship between wind speed and time and power.
# However, there does not seem to be a clear relationship between wind speed and humidity
# and wind speed and solar irradiance. There appears to be a slight relationship between
# wind speed and temperature, because we observe that higher wind speeds tend to occur
# at higher temperatures.

# vi)

par(mfrow = c(1,1))
Windspeed = data$Speed_ms[-4105]
Windspeed_lag = data$Speed_ms[-1]

plot(Windspeed, Windspeed_lag, xlab = "Wind Speed (m/s)", ylab =
"Wind Speed Lagged Values (m/s)", main = "Wind Speed vs Wind Speed Lagged Values",
cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5, col = 'darkgreen')

# There is a clear relationship between wind speed and its lagged values, where
# as wind speed increases, its lagged values also increase in what looks to be a
# linear relationship.

# vii)

Windpower = data$Power_scaled[-4105]
Windpower_lag = data$Power_scaled[-1]

plot(Windpower, Windpower_lag, xlab = "Wind Power (scaled)", ylab =
"Wind Power Lagged Values (scaled)", main = "Wind Power vs Wind Power Lagged Values",
cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5, col = 'navyblue')

# There seems to be a relationship between wind power and its lagged values,
# where as wind power increases, its lagged values also increase. There is a high
# concentration of low wind power values and low lagged values.

# viii)

data_new = data[,-1]

cor(data_new)

# There is a strong positive correlation between wind speed and wind power at a value of 0.89.
# There is also a moderate negative correlation between humidity and solar irradiance.
# There is a weak correlation between all other variables.

# ix) 

cor(Windspeed, Windspeed_lag)
cor(Windpower, Windpower_lag)

# One interesting trend and finding is that the correlation of wind speed and wind
# power with their respective lagged values is strong, at values of 0.9155812 and
# 0.8199444. This shows that the lagged values may be useful to accurately forecast.
# Wind speed also has a strong correlation with wind power at a value of 0.8885186,
# so these may also be useful to forecast speed and power.

# x)

par(mfrow = c(2,2))

plot(data$Power_scaled,Time, xlab = "Wind Power (scaled)", ylab = "Time (days)",
     main = "Wind Power Over Time", col = 'purple', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Power_scaled, data$Temperature_C, xlab = "Wind Power (scaled)", ylab = "Temperature (deg C)",
     main = "Wind Power vs Temperature", col = 'red', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Power_scaled, data$Humidity, xlab = "Wind Power (scaled)", ylab = "Humidity",
     main = "Wind Power vs Humidity (g.m^-3)", col = 'pink', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Power_scaled, data$Solar_Irradiance_Wm2, xlab = "Wind Power (scaled)", ylab = "Solar Irradiance (W/m^2)",
     main = "Wind Power vs Solar Irradiance", col = 'brown', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)

# The plots seen previously show a relationship between wind power and time.
# Similar to the plots of wind speed versus all other variables, there does not
# seem to be a clear relationship between wind power and humidity
# and wind power and solar irradiance. There appears to be a slight relationship between
# wind power and temperature, because we observe that higher wind speeds tend to occur
# at higher temperatures.

#Task 3: Pseudocode: predicting wind power given wind speed

#k-fold cross validation 
kfold 
reshuffle data: take discrete sample from data to randomize the order
store in a data frame
create vector repeating indices 1:k the size of the reshuffled data divided by k 
create empty vector to store average testing error 

for i in 1 to kfold{
  if i is equal to index in indices vector
  store those values in test vector
  all other values stored in train vector 
}
#model training using train data vector 
model 1: linear model 
model 2: exponential model
model 3: other model with lagged value correlation included 

#prediction using test data vector 
model 1: use test data, predict wind power with linearly trained model
model 2: use test data, predict wind power with exponentially trained model
model 3: use test data, predict wind power with other trained model

#error calculation 
average the error from the actual data and predicted data for each model 
store in empty vector for average error score 

#pick the model with the lowest testing error
for i in average error score vector{ 
  if error is equal to the lowest testing MSE
  output = model that generated that error 
  else 
    output = 0 
} 

#train best model on all the data 
train best model on given data set 

#generate new predictions 
use all given data to predict wind power

generate testing error of best model using all data 