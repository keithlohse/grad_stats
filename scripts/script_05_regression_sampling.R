library("tidyverse"); library("ggdark"); library("faux")

# Normal Population
X <- rnorm(100000, mean=700, sd=1)
Y <- rnorm(100000, mean=0, sd=1)


POP = data.frame(X,Y)

SAMP <- sample_n(DATA, 10)

ggplot(data=POP, mapping=aes(x = X, y=Y)) +
  geom_point(shape=21, fill="dodgerblue", alpha=0.8, size=2)+
  stat_smooth(method="lm", col="dodgerblue", se=FALSE, alpha=0.2)+
  scale_x_continuous(name = "Population of X", limits=c(-4,4)) +
  scale_y_continuous(name = "Population of Y", limits=c(-4,4)) +
  dark_theme_gray() + 
  theme(plot.title=element_text(size=15, 
                                face="bold", 
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.background = element_rect(fill="grey10"),
        panel.background = element_rect(fill="grey20"),
        plot.caption=element_text(size=16, face = "bold"),  # caption
        strip.text = element_text(size=16),
        axis.title.x=element_text(size=16, face = "bold"),  # X axis title
        axis.title.y=element_text(size=16, face = "bold"),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis title
        axis.text.y=element_text(size=16),  # Y axis title
        legend.position = "none")

cor(X,Y)
lm(Y~X)$coefficient[1]
lm(Y~X)$coefficient[2]

# We will create an empty dataframe to store our output in...
size = 10
index<-c(1:10000)
DATA<-data.frame(index) 
DATA$r<-NULL
DATA$int<-NULL
DATA$slope <- NULL

set.seed(123)
# This is the meat of the script, this for loop will create random samples 
# (based on the index you specified above)
for (i in 1:length(DATA$index)) {
  SAMP <- sample_n(POP, 10)
  
  DATA$r[i] <- cor(SAMP$X, SAMP$Y)
  DATA$int[i]<-lm(Y~X, data=SAMP)$coefficient[1]
  DATA$slope[i] <- lm(Y~X, data=SAMP)$coefficient[2]
  
}


DATA$z <- 0.5*log((1+DATA$r)/(1-DATA$r))


ggplot(data=DATA, mapping = aes(x = int)) +
  geom_histogram(fill="dodgerblue", col="black", alpha=0.8, binwidth = (max(DATA$r)-min(DATA$r))/15) +
  scale_x_continuous(name = "Distribution of Intercepts", limits=c(-2,2)) +
  dark_theme_gray() + 
  theme(plot.title=element_text(size=15, 
                                face="bold", 
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.background = element_rect(fill="grey10"),
        panel.background = element_rect(fill="grey20"),
        plot.caption=element_text(size=16, face = "bold"),  # caption
        strip.text = element_text(size=16),
        axis.title.x=element_text(size=16, face = "bold"),  # X axis title
        axis.title.y=element_text(size=16, face = "bold"),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis title
        axis.text.y=element_text(size=16),  # Y axis title
        legend.position = "none")

mean(DATA$mean)
sd(DATA$mean)




## Regression Examples ----
# Normal Population
HT <- rnorm(100000, mean=1.70, sd=0.111)
IQ <- rnorm(100000, mean=100, sd=15)


POP = data.frame(HT,IQ)

SAMP <- sample_n(DATA, 10)

ggplot(data=POP, mapping=aes(x = HT, y=IQ)) +
  geom_point(shape=21, fill="chartreuse", alpha=0.2, size=2)+
  stat_smooth(method="lm", col="dodgerblue", se=FALSE, alpha=0.2)+
  scale_x_continuous(name = "Population Heights (m)") +
  scale_y_continuous(name = "Population IQs") +
  dark_theme_gray() + 
  theme(plot.title=element_text(size=15, 
                                face="bold", 
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.background = element_rect(fill="grey10"),
        panel.background = element_rect(fill="grey20"),
        plot.caption=element_text(size=16, face = "bold"),  # caption
        strip.text = element_text(size=16),
        axis.title.x=element_text(size=16, face = "bold"),  # X axis title
        axis.title.y=element_text(size=16, face = "bold"),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis title
        axis.text.y=element_text(size=16),  # Y axis title
        legend.position = "none")

cor(X,Y)
lm(Y~X)$coefficient[1]
lm(Y~X)$coefficient[2]

# We will create an empty dataframe to store our output in...
size = 80
index<-c(1:10000)
DATA<-data.frame(index) 
DATA$r<-NULL
DATA$int<-NULL
DATA$slope <- NULL

set.seed(123)
# This is the meat of the script, this for loop will create random samples 
# (based on the index you specified above)
for (i in 1:length(DATA$index)) {
  SAMP <- sample_n(POP, size)
  
  DATA$r[i] <- cor(SAMP$HT, SAMP$IQ)
  DATA$int[i]<-lm(IQ~HT, data=SAMP)$coefficient[1]
  DATA$slope[i] <- lm(IQ~HT, data=SAMP)$coefficient[2]
  
}


DATA$z <- 0.5*log((1+DATA$r)/(1-DATA$r))


ggplot(data=DATA, mapping = aes(x = int)) +
  geom_histogram(fill="dodgerblue", col="black", alpha=0.8, binwidth = (max(DATA$int)-min(DATA$int))/30) +
  scale_x_continuous(name = "Distribution of Intercepts", limits = c(-450,510),
                     breaks=c(-250,0,250,500)) +
  dark_theme_gray() + 
  theme(plot.title=element_text(size=15, 
                                face="bold", 
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.background = element_rect(fill="grey10"),
        panel.background = element_rect(fill="grey20"),
        plot.caption=element_text(size=16, face = "bold"),  # caption
        strip.text = element_text(size=16),
        axis.title.x=element_text(size=16, face = "bold"),  # X axis title
        axis.title.y=element_text(size=16, face = "bold"),  # Y axis title
        axis.text.x=element_text(size=16),  # X axis title
        axis.text.y=element_text(size=16),  # Y axis title
        legend.position = "none")

mean(DATA$mean)
sd(DATA$mean)
