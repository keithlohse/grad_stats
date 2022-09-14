# Author: Keith Lohse, PhD, PStat
# Title: R-demonstration for a One Way Anova
# Date: 2022-08-03

library(tidyverse); library(RCurl); library(car); library(ez)

# Example 1: Single Factor with Two Levels ---
DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/grad_stats/main/data/data_THERAPY.csv",
                 stringsAsFactors = TRUE)

DATA

DATA<-rownames_to_column(DATA, var = "subID")
head(DATA)

# Using lm () ----
mod01<-lm(Mood~Drug*Psychotherapy, data=DATA)
anova(mod01)
Anova(mod01, type="III")
summary(mod01)
vif(mod01)


contrasts(DATA$Drug)
contrasts(DATA$Drug) <- contr.poly(3)
contrasts(DATA$Drug)

contrasts(DATA$Psychotherapy)
contrasts(DATA$Psychotherapy) <- contr.poly(2)
contrasts(DATA$Psychotherapy)


mod02<-lm(Mood~Drug*Psychotherapy, data=DATA)
anova(mod02)
Anova(mod02, type="III")
summary(mod02)
vif(mod02)


# Using ezANOVA() ----
ezANOVA(
  data = DATA, 
  dv = Mood, 
  wid = subID,
  within = NULL,
  between = .(Drug, Psychotherapy),
  type = 3
)
  

# Plots ----
# Box Plot 
ggplot(data=DATA, aes(x=Drug, y = Mood)) +
  geom_point(aes(fill=Psychotherapy), shape=21, size=2,
             position=position_jitterdodge(dodge.width=0.5, jitter.width = 0.2)) +
  geom_boxplot(aes(fill=Psychotherapy), col="black", outlier.shape=NA, alpha=0.2,
               position=position_dodge(width=0.5)) +
  scale_x_discrete(name = "Drug Group") +
  scale_y_continuous(name = "Mood Score") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "bottom")



# Points by Group with Error Bars
DAT_MEAN <- DATA %>% group_by(Drug, Psychotherapy) %>%
  summarize(MEAN = mean(Mood),
            SD = sd(Mood),
            N = n()) %>%
  mutate(SE = SD/sqrt(N),
         MOE = SE*qt(0.975, df=N-1, lower.tail = TRUE),
         LL = MEAN - MOE,
         UL = MEAN + MOE)

head(DAT_MEAN)

ggplot(data=DAT_MEAN, aes(x=Drug, y = MEAN)) +
  geom_errorbar(aes(ymin=LL,
                    ymax=UL, 
                    group=Psychotherapy),
                width=0.05, 
                position = position_dodge(width=0.5)) +
  geom_line(aes(group=Psychotherapy), col="black",
             position = position_dodge(width=0.5)) +
  geom_point(aes(fill=Psychotherapy), shape=21, size=5,
             position = position_dodge(width=0.5)) +
  scale_fill_grey()+
  scale_x_discrete(name = "Drug Group") +
  scale_y_continuous(name = "Mood Score") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")










