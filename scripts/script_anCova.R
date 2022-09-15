# Author: Keith Lohse, PhD, PStat
# Title: R-demonstration for a AN(C)OVA
# Date: 2022-09_05

library(tidyverse); library(RCurl); library(car); library(ez)

# Example 1: Single Factor with Two Levels ---
DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/grad_stats/main/data/data_GAME.csv",
                 stringsAsFactors = TRUE)

head(DATA)

plot(post_score~group, data=DATA)
plot(pre_score~group, data=DATA)
plot(post_score~pre_score, data=DATA)


DATA$exp.c <- as.numeric(DATA$group)-1.5
DATA$pre_score.c <- DATA$pre_score - mean(DATA$pre_score)


# Using lm () ----
mod00<-lm(post_score~exp.c, data=DATA)
Anova(mod00, type="III")
summary(mod00)


mod01<-lm(post_score~pre_score.c+exp.c, data=DATA)
anova(mod01)
Anova(mod01, type="III")
summary(mod01)
vif(mod01)
cor(DATA$pre_score.c, DATA$exp.c)
cor(DATA$pre_score.c, DATA$post_score)


# "Homogeneity of Slopes" ----
mod02<-lm(post_score~pre_score.c*exp.c, data=DATA)
anova(mod02)
Anova(mod02, type="III")
summary(mod02)
vif(mod02)


# Using ezANOVA() ----
ezANOVA(
  data = DATA, 
  dv = post_score, 
  wid = subID,
  within = NULL,
  between = group,
  between_covariates = pre_score,
  type = 3
)
  

# Plots ----
# Box Plot 
ggplot(data=DATA, aes(x=group, y = post_score)) +
  geom_point(aes(fill=group), shape=21, size=2,
             position=position_jitter(width = 0.2)) +
  geom_boxplot(aes(fill=group), col="black", outlier.shape=NA, alpha=0.2,
               position=position_dodge(width=0.5)) +
  scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "Post-Test Score") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")



# Scatter Plots more accurately showing covariance
# Box Plot 
ggplot(data=DATA, aes(x=pre_score, y = post_score)) +
  geom_point(aes(fill=group), shape=21, size=2,
             position=position_jitter(width = 0.2)) +
  stat_smooth(aes(group=group, col=group), se=FALSE,
              method="lm")+
  scale_x_continuous(name = "Pre-Test Score") +
  scale_y_continuous(name = "Post-Test Score") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "bottom")









