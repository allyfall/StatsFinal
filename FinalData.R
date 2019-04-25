# StatsFinal
###Note: you need to set working directory to calCOFI for this to work.
#data: bottle.csv and cast.csv
##reading in the csv files
#bottle is the data from each cast
bottleData <- read.csv("bottle.csv")
str(bottleData)
#cast is the metadata about the cast, like the ship speed and stuff. Not sure how much we will need this.
castData <- read.csv("cast.csv")
str(castData)

#What variables do we want to look at here? 
View(bottleData)
#We want variables:Btl_Cnt:2 Depthm:5, T_degC:6, Salnty:7, 
#O2ml_L:8 O2Sat:10, ChlorA:22, Phaeop:24 PO4uM:26 SiO3uM:28 
#NO2uM:30 NO3uM:32 NH3uM:34 C14As1:36 C14As2:39 DarkAs:42
#MeanAs:45 LightP:49 R_Depth: 50, R_TEMP:51, R_SALINITY:53, 
#R_SIGMA:54, R_O2:57 R_O2Sat:58,
#R_SIO3:59, R_PO4:60, R_NO3:61, R_NO2:62, R_NH4:63, R_CHLA:64, R_PHAEO:65, R_PRES:66
#R_SAMP:67, PH1 and PH2: 72 and 73

varint <- c(2,5,6,7,8,10,22,24,26,28,30,32,34,36,39,42,45,49,50,51,53,
            54,57,58,59,60,61,62,63,64,65,66,67,72,73)
bottleWanted <- bottleData[,varint]
#Starting some Preliminary Exploration: 
plot(T_degC~Depthm, data=bottleWanted)
plot(Phaeop~Depthm, data=bottleWanted)
plot(ChlorA~Depthm, data=bottleWanted)
#note: we have more info on ChlorA than Phaeop (Whatever that means?)
ChlorA_Data <- bottleWanted[,7]
Phaeop_Data <- bottleWanted[,8]
nutrient_variables <- c(4,5,7,9,10,11,12,13)
nut_data <- bottleWanted[,nutrient_variables]
nut_plus <- nut_data
nut_plus[,9] <- ChlorA_Data
pairs(nut_plus)
plot(log(Phaeop)~ChlorA, data=bottleWanted)
plot(log(Phaeop)~log(ChlorA), data = bottleWanted)
#so if you plot the logs of both ChlorA and Phaeop, it looks pretty linear.
#if only log of Phaeop, looks like it is exponential decay. 
#we will split into training and testing: 70% 30%