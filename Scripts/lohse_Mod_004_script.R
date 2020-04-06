# Keith Lohse
# NeuroRehabilitation Informatics Lab 2018-10-21
library("car"); library("dplyr")

setwd("C:/Users/u6015231/Box Sync/KINES_7103_data_analysis")

list.files("./Data")

DATA<- read.csv("./Data/data_ALTITUDE.csv", 
                header = TRUE, sep=",",na.strings=c("","NA"))

head(DATA)

plot(decrease~altitude, data=DATA)


# Question 2: Filling in the Standard ANOVA Table
# SSE Total 
sum((DATA$decrease-mean(DATA$decrease))^2)

df1 
df2
MSR
MSE
F_obs
pF_obs<-pf()
# Note that pf() takes the form: pf(Q, df1, df2, lower.tail = FALSE)
# Q = quantity to be evaluated
# df1 = PA - PC
# df2 = N - PA
# lower.tail = specifies with tail of the distribution is evaluated.

  


# Question 3: Sampling Distributions of F
# First, we will create a sequence of possible F values.
Fs<-seq(from=0.1, to=5, by=0.01)

# Then we can plot these F-values for different degrees of freedom.
df1<-5
df2<-20

plot(df(Fs, df1, df2)~Fs, xlab="F-Value", ylab="Density of F", 
     cex.lab=1.5, cex.axis=1.5)

# Find the F-critical value for this distribution. 
F_crit<-qf(p=0.95, df1, df2, lower.tail = TRUE)
F_crit

# Plot this value on your graph
abline(v=F_crit, col="red", lty=2, lwd=1.5)


