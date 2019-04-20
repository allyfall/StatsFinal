# StatsFinal
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
#We want variables:Btl_Cnt:2 Depthm:5, T_degC:6, Salnty:7, O2ml_L:8 O2Sat:10, ChlorA:22, Phaeop:24 PO4uM:26 SiO3uM:28 NO2uM:30 NO3uM:32 NH3uM:34 C14As1:36 C14As2:39 DarkAs:42 MeanAs:45 LightP:49 R_Depth: 50, R_TEMP:51, R_SALINITY:53, R_SIGMA:54, R_O2:57 R_O2Sat:58,
#R_SIO3:59, R_PO4:60, R_NO3:61, R_NO2:62, R_NH4:63, R_CHLA:64, R_PHAEO:65, R_PRES:66
#R_SAMP:67

#Starting some Preliminary Exploration: 