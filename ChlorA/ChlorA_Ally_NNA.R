
##Ally looks at ChlorA 
bottleData <- read.csv("bottle.csv")
#I am building MLR models for the relationship between ChlorA and 
#the nutrients, looking at relevent model diagnostics,
#and then running the model on the testing data to see how accurate 
#model is. 
#We want variables:Btl_Cnt:2 Depthm:5, T_degC:6, Salnty:7, 
#O2ml_L:8 O2Sat:10, ChlorA:22, Phaeop:24 PO4uM:26 SiO3uM:28 
#NO2uM:30 NO3uM:32 NH3uM:34 C14As1:36 C14As2:39 DarkAs:42
#MeanAs:45 LightP:49 R_Depth: 50, R_TEMP:51, R_SALINITY:53, 
#R_SIGMA:54, R_O2:57 R_O2Sat:58,
#R_SIO3:59, R_PO4:60, R_NO3:61, R_NO2:62, R_NH4:63, R_CHLA:64, R_PHAEO:65, R_PRES:66
#R_SAMP:67, PH1 and PH2: 72 and 73
#Depthm+Salnty+T_degC+R_SIGMA+PO4uM+SiO3uM+NO2uM+NO3uM+NH3uM+C14As2
varint <- c(2,5,6,7,10,22,24,26,28,30,32,34,36,39,42,45,49,50,51,53,
            54,57,58,59,60,61,62,63,64,65,66,67,72,73)
masterint <- c(5,6,7,54,26,28,30,32,34,39)
bottleWanted <- bottleData[,varint]
bottlemaster <- bottleData[,masterint]
bottleWanted <- bottleWanted[-c(which(is.na(bottleWanted$ChlorA))),]
ChlorA <- bottleWanted[,6]
plot(pH1~pH2, data=bottleWanted)
plot(C14As1~C14As2, data=bottleWanted)

#nutrient names: PO4uM:26 SiO3uM:28 
#NO2uM:30 NO3uM:32 NH3uM:34 C14As1:36 C14As2:39
nutrient_variables <- c(8:14)
nut_data <- bottleWanted[,nutrient_variables]
nut_data[,8] <- ChlorA
names(nutAndChlor[,8]) <- "ChlorA" 
#environment data: 
#Btl_Cnt:2 Depthm:5, T_degC:6, Salnty:7, 
#O2ml_L:8 O2Sat:10, DarkAs:42
#MeanAs:45 LightP:49 R_Depth: 50, R_TEMP:51, R_SALINITY:53, 
#R_SIGMA:54, R_O2:57 R_O2Sat:58, R_PRES:66
#R_SAMP:67, PH1 and PH2: 72 and 73
env_variables <- c(2:5)
env_data <- bottleWanted[,env_variables]
env_data[,5] <- ChlorA
View(env_data)


##filter for complete cases only:
d.Phaeop.nuts <- d.Phaeop.nuts[complete.cases(d.Phaeop.nuts), ] # Perf 
nut_data <- nut_data[complete.cases(nut_data), ] 
ChlorA_nut <- nut_data[,8]
env_data <- env_data[complete.cases(env_data), ]
master_test <- master_test[complete.cases(master_test),]

##training and testing data:
set.seed(2019)
n <- 3600*.7
train_ind <- sample(1:3600, size = n, replace = FALSE)
nut_train <- nut_data[train_ind,]
ChlorA_nut_test <- ChlorA_nut[-train_ind]
ChlorA_nut_train <- ChlorA_nut[train_ind]
ChlorA_train <- ChlorA[train_ind]
nut_test <- nut_data[-train_ind,]
ChlorA_test <- ChlorA[-train_ind]
master_train <- bottlemaster[train_ind,]
master_test <- bottlemaster[-train_ind,]
#training for env
n <- 220772*.7
etrain_ind <- sample(1:220772, size = n, replace = FALSE)
env_train <- env_data[etrain_ind,]
Chlor_env <- env_data[,5]
env_test <- env_data[-etrain_ind,]
Chlor_env_test <- ChlorA[-etrain_ind]
Chlor_env_train <- ChlorA[etrain_ind]

#alright. Lets start some modeling
library(leaps)
library(DMwR)
best.ChlorA.Nut <- regsubsets(ChlorA_nut~.,data=nut_data, nvmax=10)
#NutWChlor <- regsubsets(ChlorA_train~., data=nutAndChlor_train, nvmax = 10)
summary(best.ChlorA.Nut)
summary(NutWChlor)
bCN <- summary(best.ChlorA.Nut)
which.min(summary(best.ChlorA.Nut)$bic)
which.min(summary(NutWChlor)$bic)
par(mfrow=c(2,3))
plot(bCN$rss)
plot(bCN$rsq)
plot(bCN$adjr2)
plot(bCN$cp)
plot(bCN$bic)
#So the model with six features is the best: 
#P04, SI03, NO2, NO3, NH3, C14As2
#making the linear model
names(nut_data)
nut_best_mod <- lm(ChlorA_nut~PO4uM+SiO3uM+NO2uM+NO3uM+NH3uM+C14As2, data=nut_data, na.action=na.omit)
nut_log_mod <- lm(ChlorA_nut~log(PO4uM)+SiO3uM+NO2uM+NO3uM+NH3uM+C14As2, data=nut_data, na.action=na.omit)
nut_log_mod2 <- lm(sqrt(ChlorA_nut)~log(PO4uM)+SiO3uM+NO2uM+NO3uM+NH3uM+C14As2, data=nut_data, na.action=na.omit)
summary(nut_best_mod)
#mrsq = .63
#how good is model fit? 
#rsq is 64% for both
summary(nut_best_mod)$r.squared
summary(nut_log_mod)$r.squared
summary(nut_log_mod2)$r.squared #huh 62.2 for this one. 
#Residual Mean Sq Error: .925.
#ChlorA_nut runs from 0-19.13
summary(nut_best_mod)$sigma 
summary(nut_log_mod)$sigma
summary(nut_log_mod2)$sigma
#normalizing rmse: so this is like 4.8% not bad...
summary(nut_best_mod)$sigma / 19.13
par(mfrow=c(2,2))
plot(nut_best_mod)
plot(nut_log_mod)
plot(nut_log_mod2)

#Diagnostics:
par(mfrow=c(2,2))
plot(nut_best_mod)

#Predicting: 
predict_nut <- predict.lm(nut_best_mod, newdata = nut_test,interval = "prediction", na.action=na.pass)
predict_nut_omit <- predict.lm(nut_best_mod, newdata = nut_test,interval = "prediction", na.action=na.omit)
par(mfrow=c(1,2))
plot(predict_nut_omit)
plot(ChlorA_test)
plot(predict_nut)
regr.eval(ChlorA_nut_test,predict_nut)
#mae: 4.18, mse:9.55 rmse: 3.09


##Enviromental Variables
par(mfrow=c(1,1))
plot(ChlorA~Depthm, data = env_data)
plot(ChlorA~Salnty, data=env_data)
env.frame <- env_train
env.frame[,19] <- ChlorA_train
env.frame <- env.frame[,c(-6:-11, -13:-16)]
#How about lets do some modeling for the enviromental variables?
best.ChlorA.Env <- regsubsets(Chlor_env~.,data=env_data, nvmax=10)
summary(best.ChlorA.Env)
bCE <- summary(best.ChlorA.Env)
which.min(summary(best.ChlorA.Env)$bic)
#6 predictors is also good for env data. pH1, pH2,
#R_SIGMA, T_decC, Depthm, Salnty
par(mfrow=c(2,3))
plot(bCE$rss)
plot(bCE$rsq)
plot(bCE$adjr2)
plot(bCE$cp)
plot(bCE$bic)
#making the linear model
names(env_data)
Env_no_pH <- lm(Chlor_env_train~Depthm+Salnty+T_degC+O2Sat, data=env_train, na.action = na.omit)
Env_mod2 <- lm(sqrt(Chlor_env_train)~Depthm+Salnty+T_degC+O2Sat, data=env_train, na.action = na.omit)
Env_mod3 <- lm(Chlor_env_train~sqrt(Depthm)+Salnty+T_degC+O2Sat, data=env_train, na.action = na.omit)
par(mfrow=c(2,2))
plot(Chlor_env_train~Salnty, data = env_train)
plot(Chlor_env_train~log(Depthm), data = env_train)
plot(Chlor_env_train~O2Sat, data = env_train)
plot(Chlor_env_train~T_degC, data = env_train)
summary(Env_no_pH)
#how good is model fit?
summary(Env_no_pH)$r.squared
#r squared of .004 --> how much the model explains the data/closeness of fit.
#13.5% is garbage for no pH. With pH is 38.4%.  
#RMSE: 1.14? and 1.2 for no pH?  
summary(Env_no_pH)$sigma
summary(Env_mod2)$r.squared #.002?
summary(Env_mod2)$sigma #.454
summary(Env_mod3)$r.squared #.004?
summary(Env_mod3)$sigma #1.21
summary(Env_mod3)
#Diagnostics:
par(mfrow=c(2,2))
plot(Env_no_pH)
plot(Env_mod2)
plot(Env_mod3)
anova(Env_no_pH)
predict_env <- predict.lm(Env_no_pH, newdata = env_test,interval = "prediction")
regr.eval(Chlor_env_test, predict_env)
#mae: 5.07, mse: 14.63, rmse: 3.825


#both nut and env? 
ChlorA_all <- bottleData[,22]
env <- env.frame[,c(-9,-7)]
master <- c(env_train,nut_train)
master <- data.frame(master)
best.ChlorA.master <- regsubsets(ChlorA_train~.,data=master, nvmax=7)
#trying to make the master model. Lets just see how it goes.
best_mod <- lm(ChlorA_train~Depthm+Salnty+T_degC+R_SIGMA+PO4uM+SiO3uM+NO2uM+NO3uM+NH3uM+C14As2, data=master, na.action = na.omit)
best_mod2 <- lm(sqrt(ChlorA_all)~Depthm+Salnty+T_degC+R_SIGMA+PO4uM+SiO3uM+NO2uM+NO3uM+C14As2, data=bottlemaster, na.action = na.omit)
summary(best_mod)
#Mult R-sq 66%, Adj 65.8
#how good is model fit?
summary(best_mod)$r.squared
#r squared of .659 
#RMSE: .894 
summary(best_mod)$sigma 
#Diagnostics:
par(mfrow=c(2,2))
plot(best_mod)
anova(best_mod)
summary(best_mod2) #mrs: .63 arsq:.6334, untransformed
#.6793 and .6791 sqrt(ChlorA_all)
summary(best_mod2)$sigma #.299
plot(best_mod2)

#shall we try to predict again?
predict_master <- predict.lm(best_mod, newdata = master_test,interval = "prediction", na.action=na.omit)
plot(predict_master)
regr.eval(master_test, predict_master)

anova(best_mod)["Residuals", "Sum Sq"]
#RSS of 2024.617

#can I model with gams? 
library(fields)
library(gam)
#gam fit:
gam_test <- gam(ChlorA_nut_train~s(PO4uM,3)+s(SiO3uM,3)+s(NO2uM,3)+s(NO3uM,3)+s(NH3uM,3)+s(C14As2,3), data = nut_train)
gam_nut <- gam::gam(ChlorA_nut~s(PO4uM,3)+s(SiO3uM,3)+s(NO2uM,3)+s(NO3uM,3)+s(NH3uM,3)+s(C14As2,3), data=nut_data)
par(mfrow=c(2,3))
plot(gam_nut, se=TRUE, col="blue")
#C14As2 looks super linear.
gam_nut2 <- gam(ChlorA_train~s(PO4uM,2)+s(SiO3uM,3)+s(NO2uM,3)+s(NO3uM,2)+s(NH3uM,3)+C14As2, data=nut_train)
plot(gam_nut2, se=TRUE, col="blue")
anova(gam_nut,gam_nut2)
anova(gam_nut2)
summary(gam_nut2)
#Null Deviance 5951, Residual Deviance 2051.5
#AIC: 6701
summary(gam_nut)
#Null: 5951, Residual 2027.5
#AIC: 6680.6
#so cubic splines are better.

