## Opening Libraries -----------------------------------------------------------
library("car"); library("tidyverse");

## Setting the Directory -------------------------------------------------------
setwd("YOUR_DIRECTORY_HERE")
# let's see what is in the Data folder
list.files("./Data")

## Opening an example from the textbook ----------------------------------------
DAT1 <- read.table("./Data/ex5.1.txt", header = TRUE, sep = "")
head(DAT1)

## Getting R to run this regression model for us ----
# Create the regression model using the lm() function
mod1<-lm(Internet~College, data=DAT1)

# Print the details of the model to the screen
summary(mod1)

# Print the ANOVA table for the model to the screen
Anova(mod1, type="III")

# Create a plot visualizing the data and the results of your regression model
plot(Internet~College, data=DAT1, pch=21, bg="cyan",
     xlab="College Graduation %", ylab="Household Internet%",
     xlim=c(0,100), ylim=c(0,100))
abline(mod1, col="red", lty=2, lwd=2)



## Question 2 ----
# Now let's try calculating the slopes and intercepts by hand.
mean(DAT1$Internet)

mean(DAT1$College)


DAT1$x_res<-DAT1$College-mean(DAT1$College)
DAT1$y_res<-DAT1$Internet-mean(DAT1$Internet)
DAT1$xy_cov<-DAT1$x_res*DAT1$y_res
sum(DAT1$xy_cov)

sum((DAT1$College-mean(DAT1$College))^2)

DAT1$x_var<-DAT1$x_res^2
sum(DAT1$xy_cov)/sum(DAT1$x_var)

mean(DAT1$Internet)-0.8618517*mean(DAT1$College)


# Changing the Scale of X ----
# What happens to our linear model if we change the scale of x?
DAT1$College.p<-DAT1$College/100


mod4<-lm(Internet~College.p, data=DAT1)
summary(mod4)

plot(Internet~College.p, data=DAT1, pch=21, bg="cyan",
     xlab="College Proportion", ylab="Household Internet%",
     xlim=c(0,1), ylim=c(0,100))
abline(mod4, col="purple", lty=2, lwd=2)


# Creating a mean-centered version of the college variable ----
# What happens to our linear model if we center X around it's mean?
mean(DAT1$College)
DAT1$College.c<-DAT1$College-mean(DAT1$College)

mod2<-lm(Internet~College.c, data=DAT1)
summary(mod2)

plot(Internet~College.c, data=DAT1, pch=21, bg="cyan",
     xlab="College % - mean%", ylab="Household Internet%",
     xlim=c(-100,0), ylim=c(0,100))
abline(mod2, col="blue", lty=2, lwd=2)




# Question 3: Statistical Inferences about B_1 ----
# In Model C, we predict the mean for each score:
DAT1$Y_c<-mean(DAT1$Internet)
head(DAT1)

DAT1$e_c<-DAT1$Internet-DAT1$Y_c
head(DAT1)

# Model C SSE
SSE_C<-sum(DAT1$e_c^2)
SSE_C


# In Model A, we will base our prediction on the college graduation rates:
lm(Internet~College, data=DAT1)

DAT1$Y_a<-47.912+0.862*(DAT1$College)
head(DAT1)

DAT1$e_a<-DAT1$Internet-DAT1$Y_a
head(DAT1)


# Model A SSE
SSE_A<-sum(DAT1$e_a^2)
SSE_A

# PRE for model comparison
PRE<-(SSE_C-SSE_A)/SSE_C
PRE

# What does the null F-distribution look like for these degrees of Freedom?
x<-seq(from=0.1, to=10, by=0.1)

plot(df(x, 1, 48)~x, xlab="F-Value", ylab="Density of F", 
     cex.lab=1.5, cex.axis=1.5)

# What is the F-critical value for this null distribution?
# Use a cut-off of 5% for the Type 1 error rate:
qf(p=0.05, 1,48, lower.tail = FALSE)

# Let's add this critical value to our plot:
abline(v=4.042652, col="red", lty=2, lwd=1.5)


# Calculation of F_observed
# df1 = PA - PC
df1 <- 2-1
df1

# df2 = N - PA
df2 <- length(DAT1$Internet)-2
df2

F_obs<-(PRE/df1)/((1-PRE)/df2)
F_obs

pf(F_obs,1,48, lower.tail = FALSE)


summary(lm(Internet~College, data=DAT1))








