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
varint <- c(2,5,6,7,10,22,24,26,28,30,32,34,36,39,42,45,49,50,51,53,
            54,57,58,59,60,61,62,63,64,65,66,67,72,73)
bottleWanted <- bottleData[,varint]
ChlorA <- bottleWanted[,6]

#nutrient names: PO4uM:26 SiO3uM:28 
#NO2uM:30 NO3uM:32 NH3uM:34 C14As1:36 C14As2:39
nutrient_variables <- c(8:14)
nut_data <- bottleWanted[,nutrient_variables]

#environment data: 
#Btl_Cnt:2 Depthm:5, T_degC:6, Salnty:7, 
#O2ml_L:8 O2Sat:10, DarkAs:42
#MeanAs:45 LightP:49 R_Depth: 50, R_TEMP:51, R_SALINITY:53, 
#R_SIGMA:54, R_O2:57 R_O2Sat:58, R_PRES:66
#R_SAMP:67, PH1 and PH2: 72 and 73
env_variables <- c(1:5, 15:23, 31:34)
env_data <- bottleWanted[,env_variables]

##training and testing data:
set.seed(2019)
n <- 864863*.7
train_ind <- sample(1:864863, size = n, replace = FALSE)
nut_train <- nut_data[train_ind,]
ChlorA_train <- ChlorA[train_ind]
nut_test <- nut_data[-train_ind,]
ChlorA_test <- ChlorA[-train_ind]


#alright. Lets start some modeling
library(leaps)
best.ChlorA.Nut <- regsubsets(ChlorA~.,data=nut_data, nvmax=10)
summary(best.ChlorA.Nut)
bCN <- summary(best.ChlorA.Nut)
which.min(summary(best.ChlorA.Nut)$bic)
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
nut_best_mod <- lm(ChlorA_train~PO4uM+SiO3uM+NO2uM+NO3uM+NH3uM+C14As2, data=nut_train)
summary(nut_best_mod)
#how good is model fit?
summary(nut_best_mod)$r.squared
#Residual Mean Sq Error: 93.07%
summary(nut_best_mod)$sigma 
#Diagnostics:
par(mfrow=c(2,2))
plot(nut_best_mod)

#Predicting: This is not working rn. 
predict(nut_best_mod, newdata = nut_test,interval = "prediction")

#Cross validation:
best_nut_sub <- regsubsets(ChlorA_train~., data=nut_train, nvmax=7)
summary(best_nut_sub)
which.min(summary(best_nut_sub)$bic)
#still 6. Cool.

