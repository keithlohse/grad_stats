library(car); library(ggplot2); library(tidyverse); library(patchwork); library(lme4) 

setwd("C:/Users/kelop/Box/KINES_7103_data_analysis")
list.files("./data/")

DATA<-read.csv("./data/ex8.1.txt", header = TRUE, sep="\t",  
              na.strings=c("NA","NaN"," ",""), stringsAsFactors = TRUE)

head(DATA)

mod1 <- lm(SAT~Group, data=DATA)

summary(mod1)
plot(mod1)

DATA$pred <- fitted(mod1)
DATA$res <- residuals(mod1)
head(DATA)

plot(density(DATA$res))

ggplot(DATA, aes(x = Group, y = SAT)) +
  geom_point(aes(fill=Group), pch=21, size=2)+
  stat_smooth(aes(group=1), col="black", se=FALSE, method="lm", lwd=1)+
  scale_x_discrete(name = "Group") +
  scale_y_continuous(name = "SAT") +
  scale_fill_grey()+
  theme_bw()+
  theme(axis.text=element_text(size=14, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        plot.title=element_text(size=14, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=14, face="bold"),
        legend.position = "none")



ggplot(DATA, aes(x = Student, y = res)) +
  geom_line(col="grey", lwd=0.5)+
  geom_point(aes(fill=Group), pch=21, size=2)+
  scale_x_discrete(name = "Student") +
  scale_y_continuous(name = "Residuals") +
  scale_fill_grey()+
  theme_bw()+
  theme(axis.text=element_text(size=14, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        plot.title=element_text(size=14, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=14, face="bold"),
        legend.position = "none")


ggplot(DATA, aes(x = pred, y = sqrt(abs(res)))) +
  geom_point(aes(fill=Group), pch=21, size=2)+
  stat_smooth(aes(group=1), col="black", se=FALSE, method="lm", lwd=1)+
  scale_x_continuous(name = "Fitted Values") +
  scale_y_continuous(name = "sqrt(|Residuals|)") +
  scale_fill_grey()+
  theme_bw()+
  theme(axis.text=element_text(size=14, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        plot.title=element_text(size=14, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=14, face="bold"),
        legend.position = "none")



list.files("./data/")

DATA<-read.csv("./data/data_ALTITUDE.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""), stringsAsFactors = TRUE)
head(DATA)
ggplot(DATA, aes(x = altitude, y = decrease)) +
  geom_point(aes(fill=BVO2), pch=21, size=2)+
  stat_smooth(aes(group=1), col="black", se=FALSE, method="loess", lwd=1)+
  scale_x_continuous(name = "Test Altitude (m)") +
  scale_y_continuous(name = "Change in VO2 max (mL/kg/min)") +
  # scale_fill_grey()+
  theme_bw()+
  theme(axis.text=element_text(size=14, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        plot.title=element_text(size=14, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=14, face="bold"),
        legend.position = "right")


head(DATA)
mod1 <- lm(decrease~altitude+I(altitude^2), data=DATA)


plot(mod1)

DATA$pred <- fitted(mod1)
DATA$res <- residuals(mod1)
head(DATA)

plot(density(DATA$res))

ggplot(DATA, aes(x = subID, y = res)) +
  geom_line(col="grey", lwd=0.5)+
  geom_point(fill="white", pch=21, size=2)+
  #stat_smooth(aes(group=1), col="black", se=FALSE, method="lm", lwd=1)+
  scale_x_continuous(name = "Participant ID") +
  scale_y_continuous(name = "Residuals") +
  scale_fill_grey()+
  theme_bw()+
  theme(axis.text=element_text(size=14, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        plot.title=element_text(size=14, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=14, face="bold"),
        legend.position = "none")


ggplot(DATA, aes(x = pred, y = sqrt(abs(res)))) +
  geom_point(fill="white", pch=21, size=2)+
  stat_smooth(aes(group=1), col="black", se=FALSE, method="lm", lwd=1)+
  scale_x_continuous(name = "Fitted Values") +
  scale_y_continuous(name = "sqrt(|Residuals|)") +
  scale_fill_grey()+
  theme_bw()+
  theme(axis.text=element_text(size=14, color="black"), 
        legend.text=element_text(size=12, color="black"),
        legend.title=element_text(size=12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        plot.title=element_text(size=14, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=14, face="bold"),
        legend.position = "none")
