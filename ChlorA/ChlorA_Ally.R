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

#alright. Lets start some modeling