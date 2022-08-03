# Author: Keith Lohse, PhD, PStat
# Title: R-demonstration for a One Way Anova
# Date: 2022-08-03

library(tidyverse); library(RCurl); library(car); library(ez)

# Example 1: Single Factor with Two Levels ---
DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/grad_stats/main/data/data_SAT.csv",
                 stringsAsFactors = TRUE)

head(DATA)

plot(SAT~Group, data=DATA)

# Using lm () ----
mod01<-lm(SAT~Group, data=DATA)
Anova(mod01, type="III")
summary(mod01)


# Using ezANOVA() ----
ezANOVA(
  data = DATA, 
  dv = SAT, 
  wid = Student,
  within = NULL,
  between = Group,
  type = 3
)
  

# Plots ----
# Points by Group with Error Bars
DAT_MEAN <- DATA %>% group_by(Group) %>%
  summarize(MEAN = mean(SAT),
            SD = sd(SAT),
            N = n()) %>%
  mutate(SE = SD/sqrt(N),
         MOE = SE*qt(0.975, df=N-1, lower.tail = TRUE),
         LL = MEAN - MOE,
         UL = MEAN + MOE)

head(DAT_MEAN)

ggplot(data=DAT_MEAN, aes(x=Group, y = MEAN)) +
  geom_errorbar(aes(ymin=LL,
                    ymax=UL),
                width=0.05) +
  geom_point(shape=21, fill="gray", size=5) +
  scale_fill_grey()+
  scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "SAT Math Score", limits=c(550, 650)) +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")






# Example 1: Single Factor with Two Levels ---
DAT2 <- read.csv("https://raw.githubusercontent.com/keithlohse/grad_stats/main/data/data_800m.csv",
                 stringsAsFactors = TRUE)

head(DAT2)

# Using lm () ----
mod02<-lm(VO2~groups, data=DAT2)
Anova(mod02, type="III")
summary(mod02)


# Using ezANOVA() ----
ezANOVA(
  data = DAT2, 
  dv = VO2, 
  wid = sudID,
  within = NULL,
  between = groups,
  type = 3
)


# Plots ----
# Points by Group with Error Bars
DAT_MEAN <- DAT2 %>% group_by(groups) %>%
  summarize(MEAN = mean(VO2),
            SD = sd(VO2),
            N = n()) %>%
  mutate(SE = SD/sqrt(N),
         MOE = SE*qt(0.975, df=N-1, lower.tail = TRUE),
         LL = MEAN - MOE,
         UL = MEAN + MOE)

head(DAT_MEAN)

ggplot(data=DAT_MEAN, aes(x=groups, y = MEAN)) +
  geom_errorbar(aes(ymin=LL,
                    ymax=UL),
                width=0.05) +
  geom_point(shape=21, fill="gray", size=5) +
  scale_fill_grey()+
  scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "VO2 Max (mL/kg/min)", limits=c(40,80)) +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")



