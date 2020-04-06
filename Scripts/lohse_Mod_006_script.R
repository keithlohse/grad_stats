## Opening Libraries -----------------------------------------------------------
library("car"); library("tidyverse")

## Setting the Directory -------------------------------------------------------
setwd("YOUR DIRECTORY HERE")
# let's see what is in the Data folder
list.files("./Data")

## Opening an example from the textbook ----------------------------------------
dat <- read.table("./Data/ex6.2.txt", header = TRUE, sep = "")
head(dat)

# Let's start by calculating the Residual SS for our unconditional model.
m0<-lm(Weight~1, data=dat)
summary(m0)
sum(resid(m0)^2)

# Next we can condition our predictions based on a single a predictor, height.
m1<-lm(Weight~Height, data=dat)
summary(m1)


# If we want to improve our predictions, maybe we should condition them based on 
# two predictors, height and age. 
m2<-lm(Weight~Height+Age, data=dat)
summary(m2)

# Why has the coefficient for height changed?

## "Controlling for" other variables -------------------------------------------

##############################
plot(Height~Age, pch=21, bg="black", data=dat) 
# First of all, the predictions change because height and age are related.
# This means that ht and age are doing some of "the same work" when it comes 
# to predicting weight.
plot(Weight~Height, data=dat)
plot(Weight~Age, data=dat)
# We want to avoid increasing our predictions once for height and then increasing 
# our prediction again for age.

# How can we do that?

##############################
# First, we need to remove the shared variance from our different predictors.
# That is, to look at the independent effect of height on weight, we need to
# remove variation due to age from both height and weight.

WT_AGE<-lm(Weight~Age, data=dat) # WT_AGE = weight as a function of Age

dat$resWT_AGE<-resid(WT_AGE) # resWT_AGE = weight with age variance removed.

plot(Weight~Age, pch=21, bg="black", data=dat)
abline(WT_AGE)

# Similarly we can remove variation due to height from weight...
WT_HT<-lm(Weight~Height, data=dat)
dat$resWT_HT<-resid(WT_HT) # and we can save these residuals
# The part of weight that is left over after accounting for height tells us
# if someone is heavy for their height or light for their height

# ... remove variation due to height from age...
AGE_HT<-lm(Age~Height, data=dat)
dat$resAGE_HT<-resid(AGE_HT) # and we can save these residuals
# The part of Age that is left over after accounting for height tells us
# if someone is older for their height or young for their height

# ... and variation due to age from height.
HT_AGE<-lm(Height~Age, data=dat)
dat$resHT_AGE<-resid(HT_AGE) # and we can save these residuals
# The part of height that is left over after accounting for age tells us
# if someone is tall for their age or short for their age

####################################
#Plot residuals controlling for Age#
####################################
summary(lm(resWT_AGE~resHT_AGE, data=dat)) # Note age variance is removed from these variables
plot(dat$resWT_AGE~dat$resHT_AGE, bty='n', pch=21, bg="lightgreen", cex=1.5, 
     ylim=c(-40,60), xlim=c(-12,12))
abline(summary(lm(resWT_AGE~resHT_AGE, data=dat)))


#######################################
#Plot residuals controlling for Height#
#######################################
summary(lm(resWT_HT~resAGE_HT, data=dat)) # Note height variance is removed from these variables
plot(dat$resWT_HT~dat$resAGE_HT, bty='n', pch=21, bg="light blue", cex=1.5, 
     ylim=c(-40,40), xlim=c(-5,5))
abline(summary(lm(resWT_HT~resAGE_HT, data=dat)))

## Comparing the results of four different models ------------------------------
# Univariable weight as a function of height:
summary(lm(Weight~Height, data=dat))

# Univariable weight as a function of age: 
summary(lm(Weight~Age, data=dat))

# Multivariable model with both height and age.
summary(lm(Weight~Height+Age, data=dat))

# Univariable weight residuals as a function of height residuals:
summary(lm(resWT_AGE~resHT_AGE, data=dat))

# Univariable weight residuals as a function of age residuals: 
summary(lm(resWT_HT~resAGE_HT, data=dat))


## Optional: Visualizing the "plane" of best fit -----------------------------------------
# It may not be obvious, but when we are exploring the relationship between 
# three variables we are actually talking about a three dimensional space.
# We are used to looking at regression in two dimensions, y as a function of x. 
# In a multivariable GLM, however, we now have y as a function of x and z.
# Thus, instead of a line of best fit, we actually have a plane of best fit. 
# This can be kind of hard to visualize, but we can do it with the scatter3d
# function in the scatterplot3d package. 
install.packages("scatterplot3d") # Note this might take a minute... 
library("scatterplot3d") 

scatterplot3d(x=dat$Height,z=dat$Weight,y=dat$Age, main="Weight, Age, and Height", 
              pch=16, highlight.3d=TRUE, type="h", grid = TRUE, box=FALSE, 
              xlab="Height (in)", ylab="Age (yrs)", zlab="Weight (lbs)", 
              angle=55)


## Multivariable F and R-squared -----------------------------------------------
# From our mulivariable GLM, we can see that the F-observed is 22.27
summary(m2)
# Remember that this F refers to the entire model, no the individual parameters. 
# The great thing is that the exact same methods we used to calculate F and 
# r-squared from univariable regression generalize to multivariable regression.
SSerror <- sum(resid(m2)^2)
SStotal <-sum((dat$Weight-mean(dat$Weight))^2)
SSreg <- sum((fitted(m2)-mean(dat$Weight))^2)
# Alternatively, SSreg can be calculated by
# SSreg <- SStotal-SSerror
# R-squared then is just 
SSreg/SStotal

# And to calculate F, we need to convert our sums of squared errors into mean 
# squared errors.
MSreg <- SSreg/(3-1) # df = PA - PC
MSerror <- SSerror/(length(dat$Weight)-3) # df = N - PA
F_obs <- MSreg/MSerror
F_obs


