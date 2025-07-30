# Team Data Bosses
# Anne Christine, Tina Truc, Tasnim
# Project #2

## Task 1: Preliminary analysis
data = read.csv("data.csv")  #Load data
 # Display structure and summary statistics
str(data)
summary(data)
 # Explore the relationships between the variables using scatterplots
a = 1/144
Time = seq(0,28.5,a)
par(mfrow = c(2,2))
plot(data$Power_scaled,Time, xlab = "Wind Power (scaled)", ylab = "Time (days)",
     main = "Wind Power Over Time", col = 'purple', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Power_scaled, data$Temperature_C, xlab = "Wind Power (scaled)", ylab = "Temperature (deg C)",
     main = "Wind Power vs Temperature", col = 'red', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Power_scaled, data$Humidity, xlab = "Wind Power (scaled)", ylab = "Humidity",
     main = "Wind Power vs Humidity (g.m^-3)", col = 'pink', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
plot(data$Power_scaled, data$Solar_Irradiance_Wm2, xlab = "Wind Power (scaled)", ylab = "Solar Irradiance (W/m^2)",
     main = "Wind Power vs Solar Irradiance", col = 'orange', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
 # Correlation between Power_scaled and other variables
correlations = cor(data[c("Speed_ms", "Temperature_C", "Humidity", "Solar_Irradiance_Wm2", "Power_scaled")])
power_correlations = correlations[5, 1:4]
power_correlations 
 # Speed_ms        Temperature_C        Humidity         Solar_Irradiance_Wm2 
 # 0.8885186        0.2570446           -0.0675234            0.1958399
# Based on the scatter plots and correlation values: Speed_ms and Power_scaled have a strong positive correlation;
# Power_scaled and Temperature_C have a weak positive correlation; Humidity and Power_scaled have a weak negative 
# correlation; 

## Task 2: Model building
 # Split the data into training and testing sets: 90% for training, 10% for testing
set.seed(123)
test.id = sample(1:dim(data)[1],size=0.1*dim(data)[1])
testdf = data[test.id,]
testdf
traindf = data[-test.id,]
traindf

 # Polynomial Regression model:
 # 3rd degree for Wind speed, and 1st degree for Temperature
 pmod = lm(traindf$Power_scaled~traindf$Temperature_C+I(traindf$Speed_ms^2)
           +I(traindf$Speed_ms^3),traindf)
 pmod
 summary(pmod)
 # Random forest
 library(randomForest)
 rmod = randomForest(traindf$Power_scaled~traindf$Temperature_C+traindf$Speed_ms,
                     ntree=400,traindf)
 rmod
 summary(rmod)
 
 #computing training errors
 ypred_p = predict(pmod,traindf) #1 Polynomial
 ypred_r = predict(rmod,traindf) #2 Random forest
 
 #Training error (MSE) each model
 mean((traindf$Power_scaled - ypred_p)^2) #1 Polynomial 0.0008388916
 mean((traindf$Power_scaled - ypred_r)^2) #2 Random forest 0.0002313757
 
 #Computing testing errors
 ypred_pts = predict(lm(testdf$Power_scaled~testdf$Temperature_C+I(testdf$Speed_ms^2)
                        +I(testdf$Speed_ms^3),testdf),testdf) 
 ypred_rts = predict(randomForest(testdf$Power_scaled~testdf$Temperature_C+testdf$Speed_ms,
                                  ntree=400,testdf),testdf) 
 
 #Testing error (MSE) each model
 mean((testdf$Power_scaled - ypred_pts)^2) #1 Polynomial 0.0008913358
 mean((testdf$Power_scaled - ypred_rts)^2) #2 Random forest 0.000327627


 

















