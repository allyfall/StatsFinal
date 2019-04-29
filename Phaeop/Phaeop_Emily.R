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

# - Dividing into train and test sets, 70-30% split, RANDOMIZED (some part of this is year related...look into the metadata)
# Generate amount to split for each set: 
num.rows.phaeop <- nrow(d.Phaeop) # Number of values for non-NA Phaeop recordings
num.rows.phaeop

n.train <- round(nrow(d.Phaeop)*.70)
n.test <- nrow(d.Phaeop) - n.train

# Check: 
n.test + n.train == nrow(d.Phaeop) # Nice, checked

# Generate train and test subsets: 
## Going to try sample for this 
set.seed(2019)

test.inds <- sample(num.rows.phaeop, size=n.test, replace=FALSE)
test <- d.Phaeop[test.inds, ]
n.test # checked 

train <- d.Phaeop[-test.inds, ]
n.train # Checked 


## Order of predictive modeling (regression)
# - Predict based on nutrients 
    # - Going to use nutrients: 
    # PO4uM SiO3uM NO2uM NO3uM NH3uM C14As1 C14As2
# - Once I find the metadata for timing, we could also test on the LAST YEAR and subset this out, potentially. 

# Nutrients: 
nutrients <- c("PO4uM", "SiO3uM", "NO2uM", "NO3uM", "NH3uM", "C14As1", "C14As2")

nutrient.is <- which(names(d.Phaeop) %in% nutrients)

# Subset out the nutrients as an option, and to inspect some: 
d.nutrients <- d.Phaeop[,nutrient.is]

summary(d.nutrients)

# Pairs plots to inspect data 
quartz() 

pairs(Phaeop ~ PO4uM + SiO3uM + NO2uM + NO3uM + NH3uM + C14As1 + C14As2, data=d.Phaeop, na.action = na.omit)
# Initial notes: 
# - PO4uM + SiO3uM look VERY highly positively linearly correlated! 
# - PO4uM + SiO3uM + NO3uM look very highly positively linearly correlated # - C14As1 + C14As2 look very highly positively linearly correlated 


#### Get to some predictions to test: 
# (Note, need to build this on test data irl)
fit1 <- lm(Phaeop ~ PO4uM + SiO3uM + NO2uM + NO3uM + NH3uM + C14As1 + C14As2, data=train, na.action = na.omit)

summary(fit1)

preds1 <- predict(fit1, test, interval="prediction", na.action=na.omit)

summary(preds1)
summary(test$Phaeop)

length(preds1)
length(test$Phaeop)
summary(test$Phaeop)
summary(na.omit(test$Phaeop) - preds1)
