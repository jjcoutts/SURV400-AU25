# Internal consistency reliability of scales using Cronbach's alpha and McDonald's omega
# SURV400

# load required packages
# uncomment the install.packages() command if you don't already have MBESS installed
#install.packages("MBESS")
library(MBESS) # allows us to calculate alpha and omega

# read in the data
surveys <- read.csv(file = "~/SURV400-SP25/Data/scales.csv") # read csv into object called surveys
colnames(surveys)[1] <- "rse_1" # rename first column, it reads in weirdly

# summarize data
summary(surveys) # obtain means and other descriptives for all variables in data frame
str(surveys) # obtain structure/variable types and each variable head

# remove missing values
surveys <- na.omit(surveys) # remove all missing values listwise
summary(surveys) # now no more NAs

# estimate Cronbach's alpha of RSE
MBESS::ci.reliability(surveys[ ,c("rse_1", "rse_2","rse_3","rse_4","rse_5","rse_6")], type = "alpha")$est
# the alpha value is 0.59, which is quite low. Recall, the arbitrary rule of thumb is that 0.70 is adequate. This indicates that participants are not consistently responding to items. 

# estimate McDonald's omega of RSE
MBESS::ci.reliability(surveys[ ,c("rse_1", "rse_2","rse_3","rse_4","rse_5","rse_6")], type = "omega")$est
# the omega value is 0.64, which is still low. The same rule of thumb exists for omega (i.e., 0.70). Omega tends to be higher because alpha underestimates the reliability in our data. See the lecture for reasons why. 

# the issue here -- unknown to you -- is that one of the items on the RSE scale is reverse coded. Meaning some items are worded one way (e.g., "I believe in myself") and others are worded another (e.g., "I don't believe in myself at all")

# create RSE data frame to make computations easier
rse <- surveys[ ,c("rse_1", "rse_2","rse_3","rse_4","rse_5","rse_6")] # indexing variables from the data frame like this creates a data frame

# example of recoding reverse scored items for a 4 point scale
# always take the number of categories minus 1
rse$rse_1 <- 5-rse$rse_1 # since the RSE is on a 4-point scale, we want 1s to be 4s, 2s to the 3s, 3s to be 2s, and 4s to be 1s. Substracting 5 from each of these numbers achieves this.

### the leave one out approach is where you remove each item one at a time and see whether psychometric statistics such as reliability improve each time
# leave one out approach to see why reliability is bad 
# create empty results vector
omega_loo <- rep(NA, ncol(rse))

# for loop going over number of columns
for(i in 1:ncol(rse)){
  # remove each column in each step of the loop
  omega_loo[i] <- MBESS::ci.reliability(rse[,-i], type = "omega")$est
}
# print results
omega_loo # item one is the issue. The reliability increases to 0.78 without it and is well below .70 with it included and any other item removed



### end of script