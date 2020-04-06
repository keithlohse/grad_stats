## KINES 7103 Spring 2020
## Module 3: Sampling Distributions
## by Keith Lohse, PhD 

## Opening Libraries -----------------------------------------------------------
library("car"); library("tidyverse"); library("ez");

## Setting the Directory -------------------------------------------------------
setwd("YOUR DIRECTORY")

# let's see what is in the Data folder
list.files("./Data")

# Sampling from Two Different Populations
set.seed(1)

# Population 1: Normally Distributed
pop1<-rnorm(n=100000, mean=0, sd=1)
hist(pop1)
summary(pop1)

# Population 2: Uniformally Distributed
pop2<-runif(n=100000, min=-4, max=4)
hist(pop2)
summary(pop2)

# Clearly, these two populations look very different, but remember that 
# we wont often get to see populations in reality. As such, we want to use these
# simulations where we know the underlying reality to make decisions about which
# estimators we should use in practice (when we don't know the underlying reality)

index<-c(1:10000) # Set the upper limit to the number of experiments you want to run

DATA<-data.frame(index) #We will create an empty dataframe to store our output in...
DATA$mean1<-NULL
DATA$med1<-NULL
DATA$sd1<-NULL

DATA$mean2<-NULL
DATA$med2<-NULL
DATA$sd2<-NULL


for (i in 1:length(DATA$index)) {
  s1 <- sample(pop1, 20, replace = FALSE, prob = NULL)
  s2 <- sample(pop2, 20, replace = FALSE, prob = NULL)
  
  DATA$mean1[i]<-mean(s1)
  DATA$med1[i]<-median(s1)
  DATA$sd1[i]<-sd(s1)
  DATA$mean2[i]<-mean(s2)
  DATA$med2[i]<-median(s2)
  DATA$sd2[i]<-sd(s2)
}

head(DATA)

# Sampling from a normal distribution
hist(DATA$mean1, xlim=c(-3,3), main="Distribution of Sample Means")
mean(DATA$mean1)
sd(DATA$mean1)

hist(DATA$med1, xlim=c(-3,3), main="Distribution of Sample Medians")
mean(DATA$med1)
sd(DATA$med1)


# Sampling from a uniform distribution
hist(DATA$mean2, xlim=c(-3,3), main="Distribution of Sample Means")
mean(DATA$mean2)
sd(DATA$mean2)

hist(DATA$med2, xlim=c(-3,3), main="Distribution of Sample Medians")
mean(DATA$med2)
sd(DATA$med2)


# Thus, regardless of the "parent" distribution in the population, the 
# distribution of sample means will be approximately normal, and more normal 
# at larger sample sizes. Additionally, the less normal the parent population, 
# the less normal the sampling distribution will be. 

# Fortunately, however, approximately normal distributions are quite common.

# This also allows us to answer important questions. For instance, human VO_2 max
# has a mean of approximately 38 mL/kg/min, with a standard deviation of about 6.

# Let's generate a POPULATION of VO_2 Max scores that theoretically represent
# all 'healthy' human adults.  


## Plotting Sample Distributions -----------------------------------------------
set.seed(1)
pop1<-rnorm(n=100000, mean=38, sd=6)

hist(pop1, main="Hypothetical VO2 max Population", xlab="VO2 Max mL/kg/min", 
     col="cyan", breaks=20)

# What proportion of people have a VO_2 Max greater than 45?
(45-38)/6
pnorm(1.166667, mean=0, sd=1, lower.tail = FALSE)
sum(as.numeric(pop1>45))/100000

# What proportion of people have a VO_2 Max less than 30?
(30-38)/6
pnorm(-1.3333, mean=0, sd=1, lower.tail = TRUE)
sum(as.numeric(pop1<30))/100000


# Now, let's shift from thinking about people to thinking about samples.  
# We'll take two sets of samples, n = 10 and n = 50
SAMP<-data.frame(index) 
SAMP$mn10<-NULL
SAMP$mn50<-NULL


for (i in 1:length(SAMP$index)) {
  s1 <- sample(pop1, 10, replace = FALSE, prob = NULL)
  s2 <- sample(pop1, 50, replace = FALSE, prob = NULL)
  
  SAMP$mn10[i]<-mean(s1)
  SAMP$mn50[i]<-mean(s2)

}

head(SAMP)

# We now have two sample distributions, one with n = 10 and one with n = 50.

# What is the probability of having a sample mean greater than 45?
# N = 10
hist(SAMP$mn10, xlim=c(30,45), main="N = 10", col="firebrick")
summary(SAMP$mn10)
sd(SAMP$mn10)
sum(as.numeric(SAMP$mn10>45))/10000
q10<-(45-38)/sd(SAMP$mn10)
pnorm(q10, mean=0, sd=1, lower.tail = FALSE)
# Note that these probabilities don't quite match any more...

# N = 50
hist(SAMP$mn50, xlim=c(30,45), main="N = 50", col="deepskyblue")
summary(SAMP$mn50)
sd(SAMP$mn50)
sum(as.numeric(SAMP$mn50>45))/10000
q50<-(45-38)/sd(SAMP$mn50)
pnorm(q50, mean=0, sd=1, lower.tail = FALSE)

# HOWEVER! As we said, the sampling distribution is only approximtately normal,
# especially at small sample sizes. More specifically, the sampling distribution
# of means follows the t-distribution.

# Refer to the online modules for more detail, but we have already shown that the 
# variance of a sampling distribution depends on the number of obserations per
# sample. The t-distribution takes this into account, where as the normal 
# distribution is invariant to the sample size.

# Given a sufficiently large sample, the t-distribution will equal the normal
# distribution.

# N = 10
pnorm(q10, mean=0, sd=1, lower.tail = FALSE)
pt(q10, df=(10-1), lower.tail=FALSE)

# N = 50
pnorm(q50, mean=0, sd=1, lower.tail = FALSE)
pt(q50, df=(10-1), lower.tail=FALSE)


## Generating your own figure to show different t-distributions ----
t.values <- seq(-4,4,.1)
plot(x = t.values,y = dt(t.values,3), type = "l", lty = 1, col="black", 
     ylim = c(0,.4), xlab = "t", ylab = "f(t)")
lines(t.values,dt(t.values,10),lty = 2, col="red")
lines(t.values,dt(t.values,50),lty = 3, col="blue")
lines(t.values,dnorm(t.values),lty = 1, col="grey", lwd=2)

legend("topright", title = "df",
       legend = c(expression(infinity),"50", "10","3"),
       col = c("grey", "blue", "red", "black"),
       lty = c("solid","dashed","dotted"), bty = "n")
