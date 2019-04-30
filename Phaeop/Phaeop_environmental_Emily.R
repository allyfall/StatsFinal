##### 
# Emily Volk
# Statistical Learning, Spring 2019 
# Final Project
# Description: I analyze Phaeop q in relation to other environmental/nutrition data 
##### 

# Load data: 
# Load bottle data: 
bottle.d <- read.csv("../data/bottle.csv")

# Store comments separate so we can remove them from analysis to check NA rows 
bottle.comments <- bottle.d$DIC.Quality.Comment

# Remove comment column:  
bottle.d <- bottle.d[,-74]

#### Phaeop analysis: Trying to predict Phaeop... amt as an indicator of biomass (look into). Using nutrient data to predict
names(bottle.d)

# Taking out Phaeop = NA rows: 
which(is.na(bottle.d$Phaeop))
summary(bottle.d[-c(which(is.na(bottle.d$Phaeop))),]$Phaeop)

# Perf. Save in another data frame, only the rows where Phaeop has a value: 
d.Phaeop <- bottle.d[-c(which(is.na(bottle.d$Phaeop))),]
d.Phaeop <- d.Phaeop[d.Phaeop$Phaeop>=0,]
summary(d.Phaeop)
str(d.Phaeop)
names(d.Phaeop)

#### Modeling with Environmental factors!! ####### 
# Environmental data points using: temp, depth, salinity, oxygen aka T_degC, Salnty, O2ml_L
environmental <- c("T_degC", "Salnty", "O2ml_L")

enviro.is <- which(names(d.Phaeop) %in% environmental)

# Subset out environmental columns of interest: 
d.enviro <- d.Phaeop[,enviro.is]

# Make a new data frame of response and environmental predictors: 
d.Phaeop.e <-cbind("Phaeop" = d.Phaeop$Phaeop, d.enviro)

# Only going to use complete cases here for analysis!!! 
d.Phaeop.e <- d.Phaeop.e[complete.cases(d.Phaeop.e), ]
# Tons of these! Great! 

## Divide into test and train sets using 70-30% split 
num.rows.d <- nrow(d.Phaeop.e)

n.train.e <- round(nrow(d.Phaeop.e)*.70)
n.test.e <- nrow(d.Phaeop.e) - n.train.e

# Check: 
n.test.e + n.train.e == nrow(d.Phaeop.e)

set.seed(2019)

test.e.inds <- sample(num.rows.d, size=n.test.e, replace=FALSE)
length(test.e.inds) == n.test.e

test.e <- d.Phaeop.e[test.e.inds, ]
nrow(test.e) == n.test.e # checked 

train.e <- d.Phaeop.e[-test.e.inds, ]
nrow(train.e) == n.train.e # Checked 

###### Modeling 

# Pairs plots to inspect data 
quartz() 
pairs(Phaeop~., data=train.e)
# Hm. None of these look very great 
# Temp and O2 most correlated out of these ones 
# Transform response: 
summary(train.e$Phaeop)
summary(bottle.d$Phaeop)
boxplot(bottle.d$Phaeop)
summary(sqrt(train.e$Phaeop))
# I think negative Phaeop values are a mistake... remove these 
pairs(sqrt(Phaeop) ~ ., data=train.e)

### First model: all 
fit1.e <- lm(sqrt(Phaeop) ~ ., data=train.e)
summary(fit1.e) # Looks good for significance, terrible R-squared 

vif(fit1.e) # Eh, fine. Temp and O2 look related, but still running 
plot(fit1.e)

## Variable selection: 
# Best subsets selection: 
library(leaps)
### Predictions: 
best.sub <- regsubsets(sqrt(Phaeop) ~ ., data=train.e, nvmax=3)

names(train.e)

sb.e <- summary(best.sub)
plot(sb.e$rsq) # Sweet, choose 3 
plot(sb.e$bic)

## A little prediction: 
pred.e <- as.data.frame(predict.lm(fit1.e, test.e, interval="prediction"))
head(pred.e)

actuals.preds.e <- data.frame(cbind(actuals=test.e$Phaeop, predicteds=pred.e$fit)) 

install.packages("DMwR")
library(DMwR)

regr.eval(actuals.preds.e$actuals, actuals.preds.e$predicteds)

summary(fit1.e)
