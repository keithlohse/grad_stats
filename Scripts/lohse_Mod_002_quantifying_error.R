## KINES 7103 Spring 2020
## Quantifying Errors
## by Keith Lohse, PhD 

## Setting the Directory -------------------------------------------------------
setwd("YOUR_DIRECTORY")

# let's see what is in the Data folder
list.files("./Data")

## Opening an example from the textbook ----------------------------------------
DAT1 <- read.table("./Data/ex1.1.txt", header = TRUE, sep = "")
head(DAT1)

median(DAT1$Internet)
mean(DAT1$Internet)


hist(DAT1$Internet)
abline(v=73, lty=2, col="red")
abline(v=72.806, lty=2, col="blue")

## Calculating the Sum of Absolute Errors --------------------------------------
DAT1$res1<-DAT1$Internet-median(DAT1$Internet)
summary(DAT1$res1)
hist(DAT1$res1)

DAT1$AE<-abs(DAT1$res1)
summary(DAT1$AE)
hist(DAT1$AE)

head(DAT1)

sum(DAT1$AE)


## Calculating the Sum of Squared Errors --------------------------------------
DAT1$res2<-DAT1$Internet-mean(DAT1$Internet)
summary(DAT1$res2)
hist(DAT1$res2)

DAT1$SE<-DAT1$res2^2
summary(DAT1$SE)
hist(DAT1$SE)

head(DAT1)

sum(DAT1$SE)

