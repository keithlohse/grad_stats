## Opening Libraries -----------------------------------------------------------
library("car"); library("dplyr"); library("ez");library("ggplot2")

## Setting the Directory -------------------------------------------------------
setwd("C:/Users/u6015231/Box Sync/KINES_7103_data_analysis")
# let's see what is in the Data folder
list.files("./Data")

DATA <- read.csv("./Data/data_ANSCOMBE.csv", header = TRUE, sep = ",")
head(DATA)

# Visualizing the Data
g1<- ggplot(GROUP3, aes(x = xVal, y = yVal)) +
    geom_point(aes(fill=as.factor(group)), pch=21, color="black", size=2)+
  stat_smooth(aes(col=as.factor(group)), method="lm", se=FALSE, lwd=1)+
  #facet_wrap(~group, ncol=2)+
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
lm(yVal~xVal, data=DATA)$coefficients[1]

COEFS<-DATA %>%
  group_by(group) %>%
  summarise(Intercept=lm(yVal~xVal, data=DATA)$coefficients[1],
            Slope=lm(yVal~xVal, data=DATA)$coefficients[2])
COEFS


## Group 2 ----
DATA[DATA$group==2,]

GROUP2<-subset(DATA, group==2)

mod2<-lm(yVal~xVal, data=GROUP2)
Anova(mod2, type="III")
summary(mod2)
plot(mod2)

## Group 3 ----
DATA[DATA$group==3,]

GROUP3<-subset(DATA, group==3)

mod3<-lm(yVal~xVal, data=GROUP3)
Anova(mod3, type="III")
summary(mod3)
plot(mod3)

GROUP3_EXC<-subset(GROUP3, X != 25)
GROUP3_EXC

mod3B<-lm(yVal~xVal, data=GROUP3_EXC)
Anova(mod3B, type="III")
summary(mod3B)
plot(mod3B)

