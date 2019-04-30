##### 
# Emily Volk
# Statistical Learning, Spring 2019 
# Final Project
# Description: I analyze Phaeop q in relation to other environmental/nutrition data 
##### 
# Load data: 
# Load bottle data: 
bottle.d <- read.csv("../data/bottle.csv")

# Inspect data 
str(bottle.d)
summary(bottle.d)

# Expl. analysis: 
quartz()
names(bottle.d)

# Store comments separate so we can remove them from analysis to check NA rows 
bottle.comments <- bottle.d$DIC.Quality.Comment

# Remove comment column:  
bottle.d <- bottle.d[,-74]

sum(as.logical(is.na(bottle.d[1])==FALSE))
# No fully NA rows 
#### ONLY GOING TO USE COMPLETE CASES - taking out any rows with NA inputs!! This will happen later when I subset out nutrient data for Phaeop analysis 

# Exploratory analysis continued:
plot(log(bottle.d$Phaeop))

## Cast data: 
cast.d <- read.csv("../data/cast.csv")
summary(cast.d)
# These are more spatial values, about the sampling locations +++ ## GREAT! For later...

#### Phaeop analysis: Trying to predict Phaeop... amt as an indicator of biomass (look into). Using nutrient data to predict
names(bottle.d)

# Inspect Phaeop vals: 
summary(bottle.d$Phaeop)
model <- lm(Phaeop~., data = bottle.d)
# HM, interesting!! There are a lot of NA's in this variable. Should I take them out or leave them in?? Probably take out...

# Taking out Phaeop = NA rows: 
which(is.na(bottle.d$Phaeop))
summary(bottle.d[-c(which(is.na(bottle.d$Phaeop))),]$Phaeop)

# Perf. Save in another data frame, only the rows where Phaeop has a value: 
d.Phaeop <- bottle.d[-c(which(is.na(bottle.d$Phaeop))),]

summary(d.Phaeop)
str(d.Phaeop)
# Nice: All values except for metadata, somewhat, are num or int values 

plot(d.Phaeop$Phaeop) 
# Comments on plot: Interesting! Pretty evenly spread out! Some outliers, near beginning it seems.

## Subset out nutrients: 
# Nutrients: 
nutrients <- c("PO4uM", "SiO3uM", "NO2uM", "NO3uM", "NH3uM", "C14As1", "C14As2")

nutrient.is <- which(names(d.Phaeop) %in% nutrients)

# Subset out the nutrients as an option, and to inspect some: 
d.nutrients <- d.Phaeop[,nutrient.is]

summary(d.nutrients) # A lot of NA's. Will fix 

# Make a new data frame of response and nutrient predictors: 
d.Phaeop$Phaeop

d.Phaeop.nuts <-cbind("Phaeop" = d.Phaeop$Phaeop, d.nutrients)

summary(d.Phaeop.nuts)

# -----> NOW only use cases with complete nutrient data, from here, for analysis!
sum(complete.cases(d.Phaeop.nuts)) 

d.Phaeop.nuts <- d.Phaeop.nuts[complete.cases(d.Phaeop.nuts), ] # Perf 
# Will have this number (3600) data points in the total  analysis

# - Dividing into train and test sets, 70-30% split, RANDOMIZED (some part of this is year related...look into the metadata)
# Generate amount to split for each set: 
num.rows <- nrow(d.Phaeop.nuts) # Number of values for non-NA Phaeop recordings
num.rows

n.train <- round(nrow(d.Phaeop.nuts)*.70)
n.test <- nrow(d.Phaeop.nuts) - n.train

# Check: 
n.test + n.train == nrow(d.Phaeop.nuts) # Nice, checked

# Generate train and test subsets: 
## Going to try sample for this 
set.seed(2019)

test.inds <- sample(num.rows, size=n.test, replace=FALSE)
# Check: 
length(test.inds) == n.test

test <- d.Phaeop.nuts[test.inds, ]
nrow(test) == n.test # checked 

train <- d.Phaeop.nuts[-test.inds, ]
nrow(train) == n.train # Checked 

##### Order of predictive modeling (regression)
# - Predict based on nutrients 
    # - Going to use nutrients: 
    # PO4uM SiO3uM NO2uM NO3uM NH3uM C14As1 C14As2
# - Once I find the metadata for timing, we could also test on the LAST YEAR and subset this out, potentially. 

# Pairs plots to inspect data 
quartz() 

pairs(Phaeop ~ PO4uM + SiO3uM + NO2uM + NO3uM + NH3uM + C14As1 + C14As2, data=d.Phaeop.nuts)

pairs(sqrt(Phaeop) ~ PO4uM + SiO3uM + NO2uM + NO3uM + NH3uM + C14As1 + C14As2, data=d.Phaeop.nuts)

# Initial notes: 
# - PO4uM + SiO3uM look VERY highly positively linearly correlated! 
# - PO4uM + SiO3uM + NO3uM look very highly positively linearly correlated # - C14As1 + C14As2 look very highly positively linearly correlated 
# - <3 Log transform everything!!! Clustered around 0/at pretty vastly diff scales 
# - Cols 4-8 need transformation to ameliorate clustering on LHand side of the plot
# - C14 data needs MAJOR log transform to fit vastly different scale. Cols 3 and 5 could also use this 

# Pairs plot for transformed data: 
pairs(log(Phaeop) ~ log(PO4uM) + log(SiO3uM) + log(NO2uM) + log(NO3uM) + log(NH3uM) + log(C14As1) + log(C14As2), data=d.Phaeop.nuts)

quartz()
pairs(sqrt(Phaeop) ~ log(PO4uM) + log(SiO3uM) + log(NO2uM) + log(NO3uM) + log(NH3uM) + log(C14As1) + log(C14As2), data=d.Phaeop.nuts)

# NOTE: Some data points in these are 0. Use an alternative transformation than log to deal with these 

## BOXPLOTS to check for outliers:

boxplot(d.Phaeop.nuts$Phaeop) # Two outliers, but may be fine
boxplot(d.Phaeop.nuts$PO4uM) # Top-heavy dist, but okay 
boxplot(d.Phaeop.nuts$SiO3uM) # Eh, fine 
boxplot(d.Phaeop.nuts$NO2uM) # not familiar enough with the data to know...
boxplot(d.Phaeop.nuts$NH3uM) 
boxplot(d.Phaeop.nuts$C14As1) 
boxplot(d.Phaeop.nuts$C14As2) 

summary(log(train$Phaeop))

summary(train)

### Transformations: 
pairs(sqrt(Phaeop) ~ log(PO4uM), data=d.Phaeop.nuts)
summary(d.Phaeop.nuts$PO4uM)
summary(sqrt(d.Phaeop.nuts$PO4uM))
# Doesn't change that much 

summary(train)

summary(log(train$Phaeop)) # Zero vals 
summary(log(train$Phaeop + 1))
summary(sqrt(train$Phaeop)) # SQRT RESPONSE
plot(sqrt(train$Phaeop))
plot(log(train$Phaeop+1))

summary(train$PO4uM) # But also pretty fine untransformed - KEEP UNTRANSFORMED 
summary(log(train$PO4uM)) # This one is fine 

summary(train$SiO3uM) # Bad init  
summary(log(train$SiO3uM)) # Zero vals 
summary(log(train$SiO3uM + 1)) # DO THIS ONE 
summary(sqrt(train$SiO3uM))

summary(train$NO2uM) # Okay untransformed 
plot(train$NO2uM)
summary(log(train$NO2uM)) # Zero vals 
summary(log(train$NO2uM + 1))
summary(sqrt(train$NO2uM))
pairs(sqrt(Phaeop) ~ log(NO2uM+1), data=train)
plot(log(train$NO2uM + 1))
plot(sqrt(train$NO2uM)) # Either 

pairs(sqrt(Phaeop) ~ NO2uM, data=train)
pairs(sqrt(Phaeop) ~ log(NO2uM+1), data=train)
pairs(sqrt(Phaeop) ~ sqrt(NO2uM), data=train) # May spread it out a bit better 
pairs(sqrt(Phaeop) ~ sqrt(NO2uM), data=train)

summary(train$NO3uM)
summary(log(train$NO3uM)) # Zero vals 
summary(log(train$NO3uM+1)) # Good 
summary(sqrt(train$NO3uM))

summary(train$NH3uM)
summary(log(train$NH3uM)) # Zero vals 
summary(sqrt(train$NH3uM)) # This one probably looks fine 
summary(log(train$NH3uM + 1))

summary(train$C14As1) 
summary(log(train$C14As1))
summary(log(train$C14As1 + 1)) # This one, prob

summary(sqrt(train$C14As1)) # Bad. 
summary(log(train$C14As2)) # Zero vals 
summary(log(train$C14As2 + 1)) # this one, prob 

summary(log(train))

#### Get to some predictions to test: 
# (Note, need to build this on test data irl)
### Fit on untransformed data ### 
fit.noTransform <- lm(Phaeop ~ PO4uM + SiO3uM + NO2uM + NO3uM + NH3uM + C14As1 + C14As2, data=train)

summary(fit.noTransform)

### Fit on all log-transformed data ### 
fit.logTrans <- lm(log(Phaeop) ~ log(PO4uM) + log(SiO3uM) + log(NO2uM) + log(NO3uM) + log(NH3uM) + log(C14As1) + log(C14As2), data=train)

summary(fit.logTrans)

fit.transforms <- lm(sqrt(Phaeop) ~ sqrt(PO4uM) + log(SiO3uM + 1) + sqrt(NO2uM) + log(NO3uM+1) + sqrt(NH3uM) + log(C14As1 + 1) + log(C14As2 + 1), data=train)

summary(fit.transforms)
summary(fit.noTransform)

# Variance Inflation Factors (VIFs): 
library(car)
vif(fit.transforms) # Some of these are large 
# Strong suggestion of multi-colinearity... 

cor(train) 
# Notes from correlations: 
# - C14s highly correlated 
# - PO4, NO3, Si03 highly correlated 

summary(fit.transforms)

# Take out least significant var, also highly correlated C14
fit.6 <- lm(sqrt(Phaeop) ~ sqrt(PO4uM) + log(SiO3uM + 1) + sqrt(NO2uM) + log(NO3uM+1) + sqrt(NH3uM) + log(C14As1 + 1), data=train)

summary(fit.transforms)

summary(fit.6)
vif(fit.6)

fit.5 <- lm(sqrt(Phaeop) ~ log(SiO3uM + 1) + sqrt(NO2uM) + log(NO3uM+1) + sqrt(NH3uM) + log(C14As1 + 1), data=train)

summary(fit.5)
vif(fit.5)
# Probably time to test with some variable selections... 
## Variable selection: 
# Best subsets selection: 
library(leaps)
### Predictions: 
best.sub <- regsubsets(sqrt(Phaeop) ~ sqrt(PO4uM) + log(SiO3uM + 1) + sqrt(NO2uM) + log(NO3uM+1) + sqrt(NH3uM) + log(C14As1 + 1) + log(C14As2 + 1), data=train, nvmax=8)

summary(best.sub)

sb <- summary(best.sub)

names(sb)
sb$which
sb$rsq
plot(sb$rsq) # Nice! Levels off around 3-5
sb$adjr2
plot(sb$adjr2)
sb$rss
sb$bic
plot(sb$bic) # Nice, 3-5 again - super flatline at 5

# Evaluating models with 3, 4 and 5 covariates 
summary(fit.transforms)
sb 

fit.3 <- lm(sqrt(Phaeop) ~ sqrt(NO2uM) + log(NO3uM + 1) + log(C14As1 + 1), data = train)

summary(fit.3)

fit.4 <- lm(sqrt(Phaeop) ~ sqrt(NO2uM) + log(NO3uM + 1) + sqrt(NH3uM) + log(C14As1 + 1), data = train)

summary(fit.4) # Nice, still great 

fit.5 <- lm(sqrt(Phaeop) ~ sqrt(NO2uM) + log(NO3uM + 1) + sqrt(NH3uM) + log(C14As1 + 1), data = train)

summary(fit.5) # No detriment to 
## FINAL MODEL <3 

plot(fit.5)

###### PREDICTIONS: 
test.nutrients <- test[,-test$Phaeop]

preds.fit5 <- as.data.frame(predict(fit.5, test.nutrients, interval="prediction"))

# Errors: 
error.fit5.preds <- test$Phaeop - preds.fit5$fit

# MSE: 
mean(error.fit5.preds^2)
# RMSE 
sqrt(mean(error.fit5.preds^2))
# R-squared = cor^2
cor(actuals.preds.fit5$actuals, actuals.preds.fit5$predicteds)^2 # Doesn't explain everything....

# One way to measure accuracy: Looking at simple correlation between actual and predicted values: 
actuals.preds.fit5 <- data.frame(cbind(actuals=test$Phaeop, predicteds=preds.fit5$fit)) 
head(test$Phaeop)
head(preds.fit5)
head(actuals.preds.fit5)

correlation.accuracy.fit5 <- cor(actuals.preds.fit5)
correlation.accuracy.fit5 # Not bad!! 66%

# Can get all error metrics in one go using regr.eval() function in the DMwR package: 
install.packages("DMwR")
library(DMwR)

?regr.eval()
regr.eval(actuals.preds.fit5$actuals, actuals.preds.fit5$predicteds)



