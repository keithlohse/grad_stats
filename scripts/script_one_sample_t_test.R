# Author: Keith Lohse, PhD, PStat
# Title: R-demonstration for a one-sample t-test
# Date: 2022-07-22

library(tidyverse); library(RCurl); library(car) 

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/grad_stats/main/data/data_SAT.csv",
                 stringsAsFactors = TRUE)

head(DATA)

hist(DATA$SAT)

# One sample t-test using t.test() ----
t.test(DATA$SAT)

# Non-zero null
t.test(DATA$SAT, mu=600) # Assuming that 600 average SAT math score

# One sided hypothesis tests 
t.test(DATA$SAT, mu=600, alternative = "greater") # H0: mu <= 600
t.test(DATA$SAT, mu=600, alternative = "less") # H0: mu >= 600

# Density Plot
ggplot(data=DATA, aes(x = SAT)) +
  geom_density(fill="grey90", col="black")+
  geom_vline(xintercept=600, col="red", lty=2, lwd=1) + 
  geom_point(aes(y=0), shape=21, fill="gray", size=5) +
  scale_fill_grey()+
  scale_x_continuous(name = "SAT Math Score") +
  scale_y_continuous(name = "Density") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")

# Single Point with SD Error Bars
ggplot(data=DATA, aes(x=1, y = mean(SAT))) +
  geom_hline(yintercept=600, col="red", lty=2, lwd=1) + 
  geom_errorbar(aes(ymin=mean(SAT)-sd(SAT),
                    ymax=mean(SAT)+sd(SAT)),
                width=0.05) +
  geom_point(shape=21, fill="gray", size=5) +
  scale_fill_grey()+
  scale_x_discrete(name = NULL) +
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


# Single Point with 95% CI
qt(0.975, df=nrow(DATA)-1, lower.tail = TRUE)

DAT_MEAN <- DATA %>% summarize(MEAN = mean(SAT),
                   SD = sd(SAT),
                   N = n()) %>%
  mutate(SE = SD/sqrt(N),
         MOE = SE*qt(0.975, df=N-1, lower.tail = TRUE),
         LL = MEAN - MOE,
         UL = MEAN + MOE)

DAT_MEAN

ggplot(data=DAT_MEAN, aes(x=1, y = MEAN)) +
  geom_hline(yintercept=600, col="red", lty=2, lwd=1) + 
  geom_errorbar(aes(ymin=LL,
                    ymax=UL),
                width=0.05) +
  geom_point(shape=21, fill="gray", size=5) +
  scale_fill_grey()+
  scale_x_discrete(name = NULL) +
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

