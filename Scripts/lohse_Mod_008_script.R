## Opening Libraries -----------------------------------------------------------
library("car"); library("tidyverse"); library("ez");

## Setting the Directory -------------------------------------------------------
setwd("YOUR DIRECTORY HERE")
# let's see what is in the Data folder
list.files("./Data")

DATA <- read.csv("./Data/data_MUSCLE.csv", header = TRUE, sep = ",")
head(DATA)

# Subsetting the data into FAST and SLOW fiber types:
FAST<-subset(DATA, FIBER_TYPE=="FAST")
SLOW<-subset(DATA, FIBER_TYPE=="SLOW")

# And further restricting that to the POST test:
FAST_POST<-subset(FAST, TIME==4)
SLOW_POST<-subset(FAST, TIME==4)



## Linear Model for the Effect of Group for Fiber Count, FAST Fibers
head(FAST_POST)
summary(as.factor(FAST_POST$GROUP_STRING))

mod_fast<-lm(FIBER_COUNT~lin.c+quad.c, data=FAST_POST)
summary(mod_fast)
Anova(mod_fast, type="III")


# Doing the same thing with the ez ANOVA function
ezANOVA(
  data = FAST_POST
  , dv = FIBER_COUNT
  , wid = SUB_ID
  , within = NULL
  , between = GROUP_STRING
  , type = 3
  , detailed = TRUE
  , return_aov = TRUE
)



## Linear Model for the Effect of Group for Fiber Count, SLOW Fibers
mod_slow<-lm(FIBER_COUNT~lin.c+quad.c, data=SLOW_POST)
summary(mod_slow)
Anova(mod_slow, type="III")

# Doing the same thing with the ez ANOVA function
ezANOVA(
  data = SLOW_POST
  , dv = FIBER_COUNT
  , wid = SUB_ID
  , within = NULL
  , between = GROUP_STRING
  , type = 3
  , detailed = TRUE
  , return_aov = TRUE
)



## Game Play Example:
DAT1 <- read.csv("./Data/data_GAME.csv", header = TRUE, sep = ",")
head(DAT1)

MEAN <- DAT1 %>% group_by(group) %>%
  summarize(focused_attention = mean(FA),
            usability = mean(US),
            aesthetics = mean(AES),
            endurability = mean(END),
            novelty = mean(NOV),
            involvment = mean(INV))

as.data.frame(MEAN)

DAT1$group.c <- (as.numeric(DAT1$group)-1.5)
head(DAT1)
mod_FA<-lm(FA~group.c, data=DAT1)
summary(mod_FA)
Anova(mod_FA, type="III")

