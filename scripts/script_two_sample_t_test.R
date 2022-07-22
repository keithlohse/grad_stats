# Author: Keith Lohse, PhD, PStat
# Title: R-demonstration for a two-sample t-test
# Date: 2022-07-22

library(tidyverse); library(RCurl); library(car) 

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/grad_stats/main/data/data_SAT.csv",
                 stringsAsFactors = TRUE)

head(DATA)

plot(SAT~Group, data=DATA)

# Two sample t-test using t.test() ----
t.test(SAT~Group, var.equal=TRUE, data=DATA)

# Non-zero null
t.test(SAT~Group, var.equal=TRUE, data=DATA, mu=20) # Assuming that 600 average SAT math score

# One sided hypothesis tests 
t.test(SAT~Group, var.equal=TRUE, data=DATA, mu=15, alternative = "greater") # H0: mu <= 15
t.test(SAT~Group, var.equal=TRUE, data=DATA, mu=15, alternative = "less") # H0: mu >= 15


# Using lm () ----
mod01<-lm(SAT~Group, data=DATA)
summary(mod01)



# Plots ----
# Density Plot
ggplot(data=DATA, aes(x = Group, y=SAT)) +
  geom_boxplot(fill="grey90", col="black")+
  scale_fill_grey()+
  scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "SAT Math Score") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")


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


# Plotting Mean Difference with Error Bars
qt(0.975, df=nrow(DATA)-2, lower.tail = TRUE)

DAT_DIFF <- DAT_MEAN %>% select(-c(SE, MOE, LL, UL)) %>% 
  pivot_wider(values_from = MEAN:N, names_from = Group) %>%
  mutate(DIFF = MEAN_course - MEAN_noCourse,
         SE = sqrt((SD_course^2)/N_course + (SD_noCourse^2)/N_noCourse),
         MOE = SE*qt(0.975, df=nrow(DATA)-2, lower.tail = TRUE),
         LL = DIFF - MOE,
         UL = DIFF + MOE)

DAT_DIFF

ggplot(data=DAT_DIFF, aes(x=1, y = DIFF)) +
  geom_hline(yintercept=15, col="red", lty=2, lwd=1) + 
  geom_errorbar(aes(ymin=LL,
                    ymax=UL),
                width=0.05) +
  geom_point(shape=21, fill="gray", size=5) +
  scale_fill_grey()+
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "SAT Math Score", limits=c(-5, 60)) +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")

