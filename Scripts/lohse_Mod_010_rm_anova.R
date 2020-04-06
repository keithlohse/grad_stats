
library(tidyverse); library(ez); library(car)

# First we will read in the data from the web:
DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/data_example.csv")

# Note that if you are having any trouble reading in the file from the web, 
# you can download the data_example.csv file from Module 10 and read it into R
# directly into your computer using the code below. Be sure to uncomment it and 
# make sure the that .csv file is saved in your working directory:
# DATA <- read.csv("./YOUR FILE PATH HERE/data_example.csv", header=T)

head(DATA, 10)

# Note that time is represented by numbers, let's recode time so that it is a
# categorical factor:
DATA$time <- factor(DATA$time)

# We can get a visual description of the data here:
ggplot(DATA, aes(x = time, y = speed)) +
  geom_point(aes(fill=age_group), size=2, shape=21,
             position=position_jitterdodge(dodge.width = 0.5))+
  geom_boxplot(aes(fill=age_group), col="black", 
               alpha=0.4, width=0.5)+
  facet_wrap(~condition)+
  scale_x_discrete(name = "Trial") +
  scale_y_continuous(name = "Speed (m/s)", limits = c(0,3)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")






# Example 1: One-Way RM ANOVA for the effect of Time ----
# To focus on the effect of time, we will first need to average across the 
# different conditions to get only one observation per person at eat time point.
# Essentially, we are pretending that we ran a different experiment in which the 
# effect of condition didn't exist. 
data_TIME <- aggregate(speed ~ subID + time + age_group + group, data=DATA, FUN=mean)
data_TIME <- data_TIME %>% arrange(subID, age_group, group, time)
head(data_TIME)

# Let's plot these aggregated data to see what they look like:
ggplot(data_TIME, aes(x = time, y = speed)) +
  geom_point(aes(fill=time), pch=21, size=2,
             position=position_jitter(w=0.2, h=0))+
  geom_boxplot(col="black", alpha=0.4, width=0.5)+
  scale_x_discrete(name = "Time (Trial Number)") +
  scale_y_continuous(name = "Speed (m/s)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")

# We can than analyze these data using the aov() function:
mod1<- aov(speed ~ time + Error(subID/(time)), data=data_TIME)
summary(mod1)

sum((data_TIME$speed-mean(data_TIME$speed))^2)

# Post hoc t-tests for the effect of time:
# Means for each condition:
aggregate(speed ~ time, data=data_TIME, FUN=mean)

# Note that we can select sub-sections of the data like this:
data_TIME$speed[data_TIME$time=="1"]

# Change the values of time== to make different comparisons,
# 1v2, note paired = TRUE because this is a within-subjects comparison:
t.test(data_TIME$speed[data_TIME$time=="1"],
       data_TIME$speed[data_TIME$time=="2"],
       paired=TRUE)


# Example 2: Two-Way RM ANOVA for the effects of Time and Condition ----
# Let's plot the data to see what this subset of factors looks like:
ggplot(DATA, aes(x = time, y = speed)) +
  geom_point(aes(fill=time), size=2, shape=21, position=position_jitterdodge(dodge.width = 0.5))+
  geom_boxplot(col="black", alpha=0.4, width=0.5)+
  facet_wrap(~condition)+
  scale_x_discrete(name = "Trial") +
  scale_y_continuous(name = "Speed (m/s)", limits = c(0,3)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")

# We can than analyze these data using the aov() function:
mod2<- aov(speed ~ condition*time + Error(subID/(condition*time)), data=DATA)
summary(mod2)

sum((DATA$speed-mean(DATA$speed))^2)

# Means for each condition:
aggregate(speed ~ time+condition, data=DATA, FUN=mean)

# Example 3: Mixed Factorial Model ----
# The fully factorial model looking at both within-subject factors and one 
# between subjects factor: 

# Let's recreate our graph to see the data:
ggplot(DATA, aes(x = time, y = speed)) +
  geom_point(aes(fill=age_group), size=2, shape=21,
             position=position_jitterdodge(dodge.width = 0.5))+
  geom_boxplot(aes(fill=age_group), col="black", 
               alpha=0.4, width=0.5)+
  facet_wrap(~condition)+
  scale_x_discrete(name = "Trial") +
  scale_y_continuous(name = "Speed (m/s)", limits = c(0,3)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")

# We can code this ANOVA in two different ways. One is using the base R code:
mod3<- aov(speed ~ age_group*condition*time + Error(subID/(condition*time)), data=DATA)
summary(mod3)

sum((DATA$speed-mean(DATA$speed))^2)

# The other is using the ez ANOVA function from the ez package. Note that in this
# package you are prompted for between and within-subject variables specifically
# and the output you get is more complicated. 
ezANOVA(data = DATA, 
        dv = .(speed),
        wid = .(subID),
        within = .(condition, time),
        between = .(age_group)
)

