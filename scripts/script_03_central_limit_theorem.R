library("tidyverse"); library("ggdark")

# Normal Population
X <- rnorm(100000, mean=0, sd=1)
range(X)

ggplot(mapping = aes(x = X)) +
  geom_histogram(fill="#C9492C", col="black", alpha=0.8, binwidth = 100/15) +
  scale_x_continuous(name = "Normally Distributed Population") +
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



# We will create an empty dataframe to store our output in...
size = 150
index<-c(1:10000)
DATA<-data.frame(index) 
DATA$sd<-NULL
DATA$var<-NULL
DATA$mean <- NULL
DATA$median <- NULL

set.seed(123)
# This is the meat of the script, this for loop will create random samples 
# (based on the index you specified above)
for (i in 1:length(DATA$index)) {
  s1 <- sample(X, size, replace = FALSE, prob = NULL)
  
  DATA$sd[i] = sd(s1)
  DATA$var[i] = var(s1)
  DATA$mean[i]<-mean(s1)
  DATA$median[i]<-median(s1)
  
}

ggplot(data=DATA, mapping = aes(x = mean)) +
  geom_histogram(fill="#b2df8a", col="black", alpha=0.8, binwidth = (max(DATA$mean)-min(DATA$mean))/15) +
  scale_x_continuous(name = "Distribution of Sample Means", limits=c(25,175)) +
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



# Uniform Population
X <- runif(n=100000, min=30, max=170)
mean(X)
sd(X)

ggplot(mapping = aes(x = X)) +
  geom_histogram(fill="#C9492C", col="black", alpha=0.8, binwidth=100/15) +
  scale_x_continuous(name = "Uniformly Distributed Population") +
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



# We will create an empty dataframe to store our output in...
size = 150
index<-c(1:10000)
DATA<-data.frame(index) 
DATA$sd<-NULL
DATA$var<-NULL
DATA$mean <- NULL
DATA$median <- NULL

set.seed(123)
# This is the meat of the script, this for loop will create random samples 
# (based on the index you specified above)
for (i in 1:length(DATA$index)) {
  s1 <- sample(X, size, replace = FALSE, prob = NULL)
  
  DATA$sd[i] = sd(s1)
  DATA$var[i] = var(s1)
  DATA$mean[i]<-mean(s1)
  DATA$median[i]<-median(s1)
  
}

ggplot(data=DATA, mapping = aes(x = mean)) +
  geom_histogram(fill="#b2df8a", col="black", alpha=0.8, binwidth = (max(DATA$mean)-min(DATA$mean))/15) +
  scale_x_continuous(name = "Distribution of Sample Means", limits=c(25,175)) +
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


#####################

# Z Scores ----

X <- seq(-6,6, by=0.1)
Density <- dnorm(X, mean=0, sd=1)

DATA <- data.frame(X, Density)

ggplot(data=DATA, mapping = aes(x = X, y = Density)) +
  geom_line(col="#b2df8a", lwd=1) +
  scale_x_continuous(name = "Standard Normal Variable") +
  scale_y_continuous(name = "Density") +
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
