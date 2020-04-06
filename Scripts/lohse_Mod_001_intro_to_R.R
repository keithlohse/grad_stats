## KINES 7103 Spring 2020
## Introduction to R
## by Keith Lohse, PhD 

## Setting the Directory ------------------------------------------------

getwd()
# should be "C:/Users/krl0022/Documents" or similar

# we can then change the working directory to a specific folder for our project
setwd("C:/Users/u6015231/Box Sync/KINES_7103_data_analysis")

getwd()

# let's see what is in the data folder
list.files("./Data")

## Basic R Functions ---------------------------------------------------
x <- 2
x

x <- 2 + 2
x

y <- 3*x
y

x <- seq(1:30)
x

y <- rep(1:3,10)
y
set.seed(1)
z <- rnorm(10000,0,1)
hist(z)
z <- sample(z, 30)
z

DATA <- data.frame(x,y,z)
DATA

# "If" Statements
w = 6
if( w < 5 ) {
    d=2
    } else {
    d=10
    }

d


# "For" Statements
totalsum <- 0
for (i in 1:length(DATA$x)) {
    runsum <- DATA$x[i]
    totalsum <- totalsum + runsum
}

# Print the value of total sum to the screen
totalsum    

# In general, we are going to avoid these loops in R and rely on pre-made functions
sum(DATA$x)

# It is also important to understand how R handles vectors:
DATA$j <- DATA$x*2
head(DATA)

DATA$k <- DATA$x * DATA$y
head(DATA)

DATA$l <- DATA$x / c(2,1,2)
head(DATA)

# R uses "memberwise" operations.
# That is, vector operations are performed unit by unit
# Thus, smaller vectors will get recycled until the longer vector is exhausted.
DATA


## Reading and Writing Data --------------------------------------------
# First, we can write (i.e., "save") the table we just created
write.csv(DATA, file="./Data/example1.csv")

# Next, we can also import data into R.
# Make sure that you have saved the data_DIFF.csv file from the website
# Save this file into your "data" sub-folder and then check that it is there
list.files("./Data")

# If the file is in the data sub-folder, we can read the table into R using
# the read.table() function
DAT2 <- read.table("./Data/data_ALTITUDE.csv", header = TRUE, sep = ",")
head(DAT2)


## Installing Packages -------------------------------------------------
# Run the code below to install packages that we will use in the course
# Note this my take a while
#install.packages("swirl"); 
install.packages("tidyverse"); install.packages("car"); install.packages("ez")

#library(swirl)
library(ez)
library(car)
library(tidyverse)



## Basic Graphing -------------------------------------------------
# The base package in R also comes with some pretty sweet graphing tools

# Plotting a univariate distribution
hist(DAT2$AVO2, col="pink")

# Plotting a bivariate relationship
plot(DAT2$decrease~DAT2$altitude, pch=21,
     bg="chartreuse", cex=1.5)

# Plotting a bivariate relationship with a trend line
p1<-plot(DAT2$decrease~DAT2$altitude, pch=21,
         bg="dodgerblue", cex=1.5,
         ylab = "Change in VO2 Max (mL/kg/min)",
         xlab = "Altitude (m)")

m1<-lm(DAT2$decrease~DAT2$altitude)
p1+abline(m1)



## Dice Rolling and Simple computation -----------------------------------------
X <- c(13, 13, 11, 5, 16, 10, 8, 14, 4, 18)

hist(X, col="black", border="grey", 
     main="Distribution of Die Rolls (d20)", xlab="Values")

mean(X)
sd(X)



