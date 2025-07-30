## part 2 

data = read.csv("data.csv")#import data 
full_testdf = read.csv("testdata_p.csv") #testing data 
# GOAL: determine wind power curve

set.seed(1234)

# Cut off the wind speed (df is the data rame after cutoff)
df = subset(data, data$Speed_ms > 0.5)

# If no cut-off, let df = data
# df = data

head(df)

# number of folds
nfold = 5 

# libraries for model usage
library(caret)
library(randomForest)

# reshuffle data and create fold id vector
reshuffled.id = sample(1:dim(df)[1], size=dim(df)[1])
df_reshuffled = df[reshuffled.id, ]
fold.id = c(rep(1, dim(df_reshuffled)[1]/nfold),
            rep(2, dim(df_reshuffled)[1]/nfold),
            rep(3, dim(df_reshuffled)[1]/nfold),
            rep(4, dim(df_reshuffled)[1]/nfold),
            rep(5, dim(df_reshuffled)[1]- 4*dim(df_reshuffled)[1]/nfold)) 
            
# empty vector of length n-fold for error terms 
tr_er1 = vector(length=nfold) #training error vectors 
tr_er2 = vector(length=nfold)
tr_er3 = vector(length=nfold) 
tr_er4 = vector(length=nfold) 
tr_er5 = vector(length=nfold) 
tr_er6 = vector(length=nfold)
te_er1 = vector(length=nfold) #testing error vectors
te_er2 = vector(length=nfold)
te_er3 = vector(length=nfold)
te_er4 = vector(length=nfold)
te_er5 = vector(length=nfold)
te_er6 = vector(length=nfold)


## K-fold cross validation
for (i in 1:nfold){
  test.id = which(fold.id == i)
  testdf = df_reshuffled[test.id, ]
  traindf = df_reshuffled[-test.id, ]
  
  #model training 
  lmod = lm(Power_scaled~Speed_ms,data = traindf) # linear model 
  lmod2 = lm(Power_scaled~Temperature_C + Speed_ms,data = traindf) # linear model 2 
  exponmod = lm(Power_scaled~exp(Speed_ms), data = traindf) #exponential model 
  pmod = lm(Power_scaled ~ Temperature_C + I(Speed_ms^2) + I(Speed_ms^3), data = traindf) #polynomial model 
  rmod = randomForest(Power_scaled ~ Temperature_C + Speed_ms, ntree = 400, data = traindf) #random forest model 
  knnmod = knnreg(Power_scaled ~ Temperature_C + I(Speed_ms^2), data = traindf, k=4) #knn model 
  
  
  #predicting with training data 
  ypred1 = predict(lmod, traindf)
  ypred2 = predict(lmod2, traindf)
  ypred3 = predict(exponmod,traindf )
  ypred4 = predict(pmod,traindf)
  ypred5 = predict(rmod, traindf)
  ypred6 = predict(knnmod,traindf)
  
  #predicting with testing data 
  ypred1_ts = predict(lmod, testdf)
  ypred2_ts = predict(lmod2, testdf)
  ypred3_ts = predict(exponmod,testdf )
  ypred4_ts = predict(pmod,testdf)
  ypred5_ts = predict(rmod, testdf)
  ypred6_ts = predict(knnmod,testdf)
  
  #training MSE 
  tr_er1[i] = mean((traindf$Power_scaled - ypred1)^2)
  tr_er2[i] = mean((traindf$Power_scaled - ypred2)^2) 
  tr_er3[i] = mean((traindf$Power_scaled - ypred3)^2) 
  tr_er4[i] = mean((traindf$Power_scaled - ypred4)^2) 
  tr_er5[i] = mean((traindf$Power_scaled - ypred5)^2) 
  tr_er6[i] = mean((traindf$Power_scaled - ypred6)^2)
  
  #testing MSE 
  te_er1[i] = mean((testdf$Power_scaled - ypred1_ts)^2)
  te_er2[i] = mean((testdf$Power_scaled - ypred2_ts)^2) 
  te_er3[i] = mean((testdf$Power_scaled - ypred3_ts)^2) 
  te_er4[i] = mean((testdf$Power_scaled - ypred4_ts)^2) 
  te_er5[i] = mean((testdf$Power_scaled - ypred5_ts)^2) 
  te_er6[i] = mean((testdf$Power_scaled - ypred6_ts)^2)
  
  
}




##outcomes of model training 
# k-fold cross validation estimates of error

mod_name = c("linear", "linear2", "exponential", "polynomial", "randomforest", "knn")
training_error = round(c(sqrt(mean(tr_er1)), 
              sqrt(mean(tr_er2)), 
              sqrt(mean(tr_er3)), 
              sqrt(mean(tr_er4)), 
              sqrt(mean(tr_er5)), 
              sqrt(mean(tr_er6))),3)
df_errorscores = cbind(mod_name, training_error)
df_errorscores

testing_error = round(c(sqrt(mean(te_er1)), 
                         sqrt(mean(te_er2)), 
                         sqrt(mean(te_er3)), 
                         sqrt(mean(te_er4)), 
                         sqrt(mean(te_er5)), 
                         sqrt(mean(te_er6))),3)
df_errorscores2 = cbind(mod_name, testing_error)
df_errorscores2

#model parameters/estimates:
summary(lmod)
summary(lmod2)
summary(exponmod)
summary(pmod)
summary(rmod)
summary(knnmod)

#comments on why some models worked better than others: 
# the linear regression models 
# the exponential model 
# thr polynomial model 

#Visualization of predictions 

#original data: wind power vs. wind speed 

# if error message figure margins are too big appears use par(mar=c(5,5,7,1))

plot(df$Speed_ms, df$Power_scaled, 
     xlab = "Wind Speed (m/s)", ylab = "Wind Power (scaled)",
     main = "Wind Speed vs Wind Power", col = 'blue', 
     cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)

# tested models visualized 
points(testdf$Speed_ms,testdf$Power_scaled,lwd=2, col="red",pch=15) # visualize prediction
points(testdf$Speed_ms,ypred1_ts,lwd=2, col="orange") # visualize prediction
points(testdf$Speed_ms,ypred2_ts,lwd=2, col="green") # visualize prediction
points(testdf$Speed_ms,ypred3_ts,lwd=2, col="purple") # visualize prediction
points(testdf$Speed_ms,ypred4_ts,lwd=2, col="cyan") # visualize prediction
points(testdf$Speed_ms,ypred5_ts,lwd=2, col="magenta") # visualize prediction
points(testdf$Speed_ms,ypred6_ts,lwd=2, col="grey") # visualize prediction
legend("topleft",legend=c("Real Data","Testing Data","Linear model 1", "Linear model 2", "Exponential", "Polynomial", "Random Forest", "Knn"), 
       fill=c("blue","red", "orange", "green", "purple", "cyan","magenta", "grey"), cex=0.5)


#best model = polynomial
# The model exhibited the lowest testing RMSE. 

#retrain model with all data set from part 1 and test with data given in part 2
  new_pmod = lm(Power_scaled ~ Temperature_C + I(Speed_ms^2) + I(Speed_ms^3), data = df_reshuffled) 
  Pred_Powerscaled = predict(new_pmod,full_testdf)
  new_dataset = cbind(full_testdf, Pred_Powerscaled)
  head(new_dataset)

## visualize final predictions 

#original data: wind power vs. wind speed 
par(mfrow = c(2,2))
plot(df$Speed_ms, df$Power_scaled, xlab = "Wind Speed (m/s)", ylab = "Wind Power (scaled)",
     main = "Wind Speed vs Wind Power", col = 'blue', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
points(new_dataset$Speed_ms,Pred_Powerscaled,lwd=2, col="orange", pch=19) # final predictions
legend("topleft",legend=c("Real Data","Final Predictions"),
       fill=c("blue","orange"), cex=0.5)

plot(df$Temperature_C, df$Power_scaled, xlab = "Temperature_C", ylab = "Wind Power (scaled)",
     main = "Temperature vs Wind Power", col = 'blue', cex.lab = 1.5, cex=1.5,cex.axis=1.5, cex.main = 1.5)
points(new_dataset$Temperature,Pred_Powerscaled,lwd=2, col="pink", pch=19) # final predictions
legend("topleft",legend=c("Real Data","Final Predictions"),
       fill=c("blue","pink"), cex=0.5)




