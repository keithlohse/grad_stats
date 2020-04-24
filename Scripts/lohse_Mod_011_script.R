# Module 11/Chapter 13: Ill-Mannered Error Terms
# Keith Lohse, PhD, PStat
# 2020-04-11

## Opening Libraries -----------------------------------------------------------
library("car"); library("tidyverse"); library("ez")

## Setting the Directory -------------------------------------------------------
setwd("YOUR DIRECTORY HERE")
# let's see what is in the Data folder
list.files("./Data")

## Anscombe's Quartet ----
DATA <- read.csv("./Data/data_ANSCOMBE.csv", header = TRUE, sep = ",")
head(DATA)

# Visualizing the Data
g1<- ggplot(DATA, aes(x = xVal, y = yVal)) +
    geom_point(aes(fill=as.factor(group)), pch=21, color="black", size=2)+
  stat_smooth(aes(col=as.factor(group)), method="lm", se=FALSE, lwd=1)+
  facet_wrap(~group, ncol=2)+
  scale_x_continuous(name = "X Values") +
  scale_y_continuous(name = "Y Values") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        #axis.text.y=element_blank(),
        #axis.title.y=element_blank(),
        #axis.ticks.y=element_blank(),
        legend.position = "none")

plot(g1)

## Models ---- 
# Note that we can extract specific coefficients from our linear model using 
# $coefficients at the end of the lm() function
lm(yVal~xVal, data=DATA)$coefficients[1]

COEFS<-DATA %>%
  group_by(group) %>%
  summarise(Intercept=lm(yVal~xVal, data=DATA)$coefficients[1],
            Slope=lm(yVal~xVal, data=DATA)$coefficients[2])
COEFS

# If we want to focus on one specific group, we can subset the data like so...
## Group 2 ----
DATA[DATA$group==2,]

GROUP2<-subset(DATA, group==2)

mod2<-lm(yVal~xVal, data=GROUP2)
Anova(mod2, type="III")
summary(mod2)

# Note that if you pass a model object to the plot() function, it will create 
# several plots by default. 
# You will need to click on the console window, and then hit enter to see 
# each successive plot.
plot(mod2)

## Group 3 ----
DATA[DATA$group==3,]

GROUP3<-subset(DATA, group==3)

mod3<-lm(yVal~xVal, data=GROUP3)
Anova(mod3, type="III")
summary(mod3)

# Note that if you pass a model object to the plot() function, it will create 
# several plots by default. 
# You will need to click on the console window, and then hit enter to see 
# each successive plot.
plot(mod3)

# Excluding the unusual data point in Group 3
GROUP3_EXC<-subset(GROUP3, X != 25)
GROUP3_EXC

mod3B<-lm(yVal~xVal, data=GROUP3_EXC)
Anova(mod3B, type="III")
summary(mod3B)
plot(mod3B)






## Outlier Example from the Book ---- 
list.files("./Data")

# Incorrect values
SAT <- c(42,48,58,45,45,86,51,56,51,58,42,55,61)
HSR <- c(91,87,85,79,90,48,83,99,81,94,86,99,99)

# Corrected values
SAT_c <- c(42,48,58,45,45,48,51,56,51,58,42,55,61)
HSR_c <- c(91,87,85,79,90,86,83,99,81,94,86,99,99)
DAT2 <- data.frame(SAT, HSR, HSR_c, SAT_c)
DAT2

mod1 <- lm(SAT~HSR, data=DAT2)
plot(x=DAT2$HSR, y=DAT2$SAT,
     xlab="High School Rank",
     ylab="SAT Verbal Score",
     cex.axis=1.5, cex.lab=1.5)
abline(mod1, col="red", lty=2)

hist(DAT2$SAT, xlab="Distribution of SAT Scores", main=NULL, breaks=10)
hist(resid(mod1), xlab="Distribution of 'Raw' Residuals", main=NULL, breaks=10)

# You can see that by default, R uses standardized residuals when you 
# pass a model object to the plot function.
plot(mod1, cex.axis=1.5, cex.lab=1.5)

# To get studentized residuals, you can use the rstudent() function.
plot(x=fitted(mod1), y=sqrt(abs(rstudent(mod1))),
     ylab=expression(sqrt("|Studentized Residuals|")),
     xlab="Fitted Values",
     main="Scale-Location Plot",
     ylim=c(0,2.5))


# After creating a linear model, there are several helpful functions that allow 
# to create your own plots or similar plots.
fitted(mod1) # returns the fitted/y^ values from the model
resid(mod1) # returns the "raw" residuals from the model
rstandard(mod1) # returns the standardized residuals from the model
rstudent(mod1) # returns the studentized residuals from the model


# Compare our model with outlying data point included and removed to 
# see the effect:
mod1 <- lm(SAT~HSR, data=DAT2)
summary(mod1)

head(DAT2)
mod2 <- lm(SAT~HSR, data=DAT2[-c(6),]) # this extra bit of code drops the sixth row
summary(mod2)
