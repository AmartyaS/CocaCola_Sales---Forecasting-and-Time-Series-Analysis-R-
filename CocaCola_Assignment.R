install.packages("fpp")
library("fpp")
install.packages("readr")
library("readr")
install.packages("smooth")
library("smooth")
install.packages("forecast")
library("forecast")
install.packages("tseries")
library("tseries")
library("readxl")
install.packages("fastDummies")
library("fastDummies")

doc <- read_xlsx(file.choose())
View(doc)
tssales <- ts(doc$Sales,frequency = 4)
plot(tssales)

train <- tssales[1:38]
train <- ts(train,frequency=4)
test <- tssales[39:42]
test <- ts(test,frequency = 4)

#################################################
############### MODEL BUILDING ##################
#################################################


###### Data Driven Approaches

#HoltWinter Model with alpha value
hwa_mod <- HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hwa_mod
hwa_pred <- data.frame(predict(hwa_mod,n.ahead = 12))
hwa_pred
hwa_mape <- MAPE(hwa_pred$fit,test)*100
hwa_mape
plot(forecast(hwa_mod,h=4))

#Holtwinter Model with alpha and beta value
hwab_mod <- HoltWinters(train,alpha = 0.2, beta = 0.15,gamma = F )
hwab_mod
hwab_pred <- data.frame(predict(hwab_mod,n.ahead = 4))
hwab_pred
hwab_mape <- MAPE(hwab_pred$fit,test)*100
hwab_mape
plot(forecast(hwab_mod,h=4))

#Holtwinter Model with alpha, beta and gamma value
hwabg_mod <- HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hwabg_mod
hwabg_pred <- data.frame(predict(hwabg_mod, n.ahead=4))
hwabg_pred
hwabg_mape <- MAPE(hwabg_pred$fit,test)*100
hwabg_mape
plot(forecast(hwabg_mod,h=4))

#HoltWinter Model with optimum value of alpha
hwna_mod <- HoltWinters(train,beta = F,gamma = F)
hwna_mod
hwna_pred <- data.frame(predict(hwna_mod,n.ahead = 4))
hwna_pred
hwna_mape <- MAPE(hwna_pred$fit,test)*100
hwna_mape
plot(forecast(hwna_mod,h=4))

#HoltWinter Model with optimum value of alpha and beta
hwnab_mod <- HoltWinters(train,gamma = F)
hwnab_mod
hwnab_pred <- data.frame(predict(hwnab_mod,n.ahead = 4))
hwnab_pred
hwnab_mape <- MAPE(hwnab_pred$fit,test)*100
hwnab_mape
plot(forecast(hwnab_mod,h=4))

#HoltWinter Model with optimum value of alpha, beta and gamma
hwnabg_mod <- HoltWinters(train)
hwnabg_mod
hwnabg_pred <- data.frame(predict(hwnabg_mod,n.ahead = 4))
hwnabg_pred
hwnabg_mape <- MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape

plot(forecast(hwnabg_mod,h=4))

#Table of Mean Percentage Absolute Error(MAPE)
table <- data.frame(c("HWA_Val","HWAB_Val","HWABG_Val","HWNA_Val","HWNAB_Val","HWNABG_Val"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))
colnames(table) <- c("Models","MAPE Values")
View(table)

#Mean Percentage Absolute Error is the least with the model having optimum value of alpha, beta and gamma
hwn_mod <- HoltWinters(tssales)
hwn_mod
hwn_pred <- data.frame(predict(hwn_mod,n.ahead = 4))
hwn_pred
hwn_mape <- MAPE(hwn_pred$fit,test)*100
hwn_mape
plot(forecast(hwn_mod,h=4))


#Using ses, holt and hw function 

#Using ses function with optimum value; alpha=0.2
sesa <- ses(train,alpha = 0.2)
sesa
sesa_pred <- data.frame(predict(sesa,h = 4))
sesa_pred
plot(forecast(sesa,n.ahead=4))
sesa_mape <- MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape
#SES Without optimum value 
sesn <- ses(train,alpha=NULL)
sesn_pred <- data.frame(predict(sesn,h=4))
sesn_pred
plot(forecast(sesn,n.ahead=4))
sesn_mape <- MAPE(sesn_pred$Point.Forecast,test)*100
sesn_mape

#Using holt function with optimum values; alpha=0.2 and beta=0.15
holtab <- holt(train,alpha = 0.2,beta = 0.15)
holtab_pred <- data.frame(predict(holtab,h=4))
holtab_pred
plot(forecast(holtab,n.ahead=4))
holtab_mape <- MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape
#Without using Optimum Values
holtnab <- holt(train,alpha = NULL,beta = NULL)
holtnab_pred <- data.frame(predict(holtnab,h=4))
holtnab_pred
plot(forecast(holtnab,n.ahead=4))
holtnab_mape <- MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape

#Using HW Function with optimum values; alpha=0.2,beta=0.15 and gamma=0.05
hwv_abg <- hw(train,alpha = 0.2,beta=0.15,gamma = 0.05)
hwvabg_pred <- data.frame(predict(hwv_abg,h=4))
hwvabg_pred
plot(forecast(hwv_abg,n.ahead=4))
hwv_mape <- MAPE(hwvabg_pred$Point.Forecast,test)*100
hwv_mape
#Without using optimum values
hwvn <- hw(train,alpha=NULL,beta = NULL,gamma = NULL)
hwvn_pred <- data.frame(predict(hwvn,h=4))
hwvn_pred
plot(forecast(hwvn,n.ahead=4))
hwvn_mape <- MAPE(hwvn_pred$Point.Forecast,test)*100
hwvn_mape

#Making a table of the MAPE values of ses, holt and hw function
table2 <- data.frame(c("SES_Optimum","SES_No_Optimum","Holt_Optimum",
                       "Holt_No_Optimum","HW_Optimum","HW_No_Optimum"),
                     c(sesa_mape,sesn_mape,holtab_mape,holtnab_mape,hwv_mape,hwvn_mape))
colnames(table2) <- c("Methods","MAPE Values")
View(table2)

#Based on the table, hw function without optimum values has the least MAPE Value 
#Thus, the final model is : 
hwvnabg <- hw(tssales,alpa=NULL,beta=NULL,gamma = NULL)
plot(forecast(hwvnabg,n.ahead=4))
plot(forecast(hwvnabg,h=4))
hwvnabg_mape <- MAPE(Forecast_Values$Point.Forecast,test)*100
Forecast_Values <- data.frame(predict(hwvnabg,h=4))
hwvnabg_mape

#___________________________________________________________________________________#

######## Model Based Approaches

#Creating Dummy Variables
doc["Quarters"] <- rep(c("Q1","Q2","Q3","Q4"),times=11,length.out=42)
file <- fastDummies::dummy_cols(doc,select_columns="Quarters")
View(file)
colnames(file)

#Pre-Processing of Data
file["t"] <- c(1:42)
file["log_Sales"] <- log(file$Sales)
file["t_square"] <- (file$t)*(file$t)
trn <- file[1:36,]
tst <- file[37:42,]

#Linear Model Method
lin_mod <- lm(Sales~t, data=trn)
summary(lin_mod)
lin_pred <- data.frame(predict(lin_mod,newdata = tst,interval = "predict"))
rmse_lin <- sqrt(mean((tst$Sales-lin_pred$fit)^2,na.rm=T))
rmse_lin

#Exponential Model Method
exp_mod <- lm(log_Sales~t, data = trn)
summary(exp_mod)
exp_pred <- data.frame(predict(exp_mod,interval='predict',newdata = tst))
rmse_exp <- sqrt(mean((tst$Sales-exp(exp_pred$fit))^2,na.rm=T))
rmse_exp

#Quadratic Model Method 
qua_mod <- lm(Sales~t+t_square,data=trn)
summary(qua_mod)
qua_pred <- data.frame(predict(qua_mod,interval = 'predict',newdata = tst))
rmse_qua <- sqrt(mean((tst$Sales-qua_pred$fit)^2,na.rm=T))
rmse_qua

#Additive Seasonality Model Method
ads_mod <- lm(Sales~Quarters_Q1+Quarters_Q2+Quarters_Q3, data=trn)
summary(ads_mod)
ads_pred <- data.frame(predict(ads_mod,interval = 'predict',newdata=tst))
rmse_ads <- sqrt(mean((tst$Sales-ads_pred$fit)^2, na.rm = T))
rmse_ads

#Additive Seasonality with Quadratic Trend 
adsq_mod <- lm(Sales~t+t_square+Quarters_Q1+Quarters_Q2+Quarters_Q3, data=trn)
summary(adsq_mod)
adsq_pred <- data.frame(predict(adsq_mod,interval = 'predict',newdata = tst))
rmse_adsq <- sqrt(mean((tst$Sales-adsq_pred$fit)^2,na.rm=T))
rmse_adsq

#Multiplicative Seasonality
mul_mod <- lm(log_Sales~Quarters_Q1+Quarters_Q2+Quarters_Q3,data=trn)
summary(mul_mod)
mul_pred <- data.frame(predict(mul_mod,interval='predict',newdata = tst))
rmse_mul <- sqrt(mean((tst$Sales-exp(mul_pred$fit))^2,na.rm = T))
rmse_mul

#Creating a table of all the RMSE Values
table3 <- data.frame(c("Linear Model","Exponential Model","Quadratic Model",
                       "Additive Seasonality Model","Additive Seasonality with Quadratic Model",
                       "Multiplicative Seasonality Model"),c(rmse_lin,rmse_exp,rmse_qua,rmse_ads,
                                                             rmse_adsq,rmse_mul))
colnames(table3) <- c("Models","RMSE Values")
View(table3)

#As per the RMSE table Additive Seasonality with Quadratic Model has the least RMSE Values
adsqu_mod <- lm(Sales~t+t_square+Quarters_Q1+Quarters_Q2+Quarters_Q3,data=file)
summary(adsqu_mod)
adsqu_pred <- data.frame(predict(adsqu_mod,interval = 'predict',newdata = tst))
plot(adsqu_mod)
#Calculating ACF Plot
acf(adsqu_mod$residuals,lag.max = 10)
#calculating model for Errors
A <- arima(adsqu_mod$residuals,order=c(1,0,0))
acf(A$residuals,lag.max = 10)
errors <- data.frame(forecast(A,h=4))
future_errors <- errors$Point.Forecast

#Predicted Values
Predicted_values <- adsqu_pred+future_errors
rmse_pred <- sqrt(mean((tst$Sales-Predicted_values$fit)^2, na.rm=T))
rmse_pred
View(Predicted_values)
acf(Predicted_values$fit,lag.max = 10)

