## KINES 7103 Spring 2019
## Introduction to R
## by Keith Lohse, PhD 

## Setting the Directory ------------------------------------------------

getwd()
# should be "C:/Users/krl0022/Documents" or similar

# we can then change the working directory to a specific folder for our project
setwd("C:/Otter/KINES_7103_data_analysis/")

getwd()

# let's see what is in the data folder
list.files("./Data")

## Translating between long and wide formats ----------------------------
library(reshape2)

# Make sure that the data_WIDE.csv file is saved in your data sub-folder
data_WIDE <- read.table("./Data/data_WIDE.csv", header = TRUE, sep = ",")
head(data_WIDE)

# Wide to long:
recaste_LONG <- reshape(data = data_WIDE, varying = 5:8, v.names = "percent", timevar = "time",
                  times = 1:4, idvar = "SubID", direction = "long")
recaste_LONG

# As you can see the recaste_LONG is grouped by time, not subject ID
# What if we want all of the data from one subject together in order?
# (i.e., group by subject then by time)
recaste_LONG <- recaste_LONG[order(recaste_LONG$SubID, recaste_LONG$time), ]
recaste_LONG

# Long to Wide:
# Sometimes we will have data in long format and want it back in wide format
# Make sure that the data_WIDE.csv file is saved in your data sub-folder
data_LONG <- read.table("./data/data_LONG.csv", header = TRUE, sep = ",")
head(data_LONG)

# We can also go from long to wide format using the reshape function
recaste_WIDE <- reshape(data = data_LONG, v.names = "Percent", idvar = "SubID",
                  timevar = "Time", direction = "wide")
recaste_WIDE

## Tidy Data --------------------------------------------------------------
# We are almost always going to prefer long format over wide format because
# long format obeys the cardinal rule of tidy data:
# One observation per row, one variable per column,

# In wide data, our time variable will always be conflated with our dependent 
# measures. Conversely, in long format, we have a single time variable and
# then multiple DVs that are either static (i.e., do not change over time) or
# dynamic (i.e., they do vary over time).

# Sub-setting our data
# The most basic sub-setting functions involve square brackets []
# For instance, to select just the score for Participant 1 we could do:
mean(data_LONG$Percent[data_LONG$SubID == 1])
# Let's confirm that this is the mean for Particpant 1:
head(data_LONG)
mean(c(0.50,0.64,0.92,0.72))

# For more complicated operations, we can use packages like dplyr:
library(dplyr)

by_sub<-summarize(group_by(data_LONG,SubID), 
                   average = mean(Percent),
                   diff = max(Percent)-min(Percent),
                   stdev = sd(Percent),
                   count = n())

by_sub 

by_sub<-as.data.frame(by_sub)
by_sub





