## KINES 7103 Spring 2020
## Module 2: Degrees of Freedom
## by Keith Lohse, PhD 

## Setting the Directory -------------------------------------------------------
setwd("YOUR_DIRECTORY")

# let's see what is in the Data folder
list.files("./Data")

## Why do we divide by N-1 instead of N? ---------------------------------------
# First we will simulate a POPULATION of data:
set.seed(1)

POP<-rnorm(n=100000, mean=0, sd=1)
hist(POP)
mean(POP)
sd(POP)



index<-c(1:10000) # Set the upper limit to the number of experiments you want to run

SAMP10<-data.frame(index) #We will create an empty dataframe to store our output in...
SAMP10$mn<-NULL
SAMP10$s_n <- NULL
SAMP10$s_n1 <- NULL


for (i in 1:length(SAMP10$index)) {
  s1 <- sample(POP, 10, replace = FALSE, prob = NULL)

  SAMP10$mn[i]<-mean(s1)
  SAMP10$s_n[i]<- sum((s1-mean(s1))^2)/10
  SAMP10$s_n1[i] <- sd(s1)
}

head(SAMP10)

hist(SAMP10$mn)
hist(SAMP10$s_n)
hist(SAMP10$s_n1)

mean(SAMP10$mn)
mean(SAMP10$s_n)
mean(SAMP10$s_n1)

## Which of our SD calculations is a better approximation of the POP SD? -------


# Let's try the same thing again with 50 oberservations per sample -------------
set.seed(1)
index<-c(1:10000) # Set the upper limit to the number of experiments you want to run

SAMP50<-data.frame(index) #We will create an empty dataframe to store our output in...
SAMP50$mn<-NULL
SAMP50$s_n <- NULL
SAMP50$s_n1 <- NULL


for (i in 1:length(SAMP50$index)) {
  s1 <- sample(POP, 50, replace = FALSE, prob = NULL)
  
  SAMP50$mn[i]<-mean(s1)
  SAMP50$s_n[i]<- sum((s1-mean(s1))^2)/50
  SAMP50$s_n1[i] <- sd(s1)
}


head(SAMP50)

hist(SAMP50$mn)
hist(SAMP50$s_n)
hist(SAMP50$s_n1)

mean(SAMP50$mn)
mean(SAMP50$s_n)
mean(SAMP50$s_n1)

## Which of our SD calculations is a better approximation of the POP SD? -------



## Sampling Distribution of the Mean -------------------------------------------
# Let us now turn our attention to the distribution of sample means.
hist(SAMP10$mn, main="n=10 per group", col = "pink", xlim=c(-1,1))
hist(SAMP50$mn, main="n=50 per group", col="dodgerblue", xlim=c(-1,1))

# Calculate the Mean and Standard Deviation for each of these distributions:
# Mean of Means
mean(SAMP10$mn)
mean(SAMP50$mn)

# SD of Means
sd(SAMP10$mn)
sd(SAMP50$mn)

# How does the SD of means relate to the original SD in the POPULATION?
# 1/x = sd(SAMP10$mn)
# Solve for X
1/sd(SAMP10$mn)
1/sd(SAMP50$mn)

# Now consider that the standard deviation is the square root of the variance.
# 1/x = sd(SAMP10$mn)^2
# Solve for X
1/(sd(SAMP10$mn)^2)
1/(sd(SAMP50$mn)^2)

# Do these (approximate) numbers look familiar?

# Can you come up with a formula to approximate the standard deviation 
# of the distribution of sample means?
