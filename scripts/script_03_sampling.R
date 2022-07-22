library("tidyverse"); library("ggdark")


X <- rnorm(100000, mean=0, sd=1)

ggplot(mapping = aes(x = X)) +
  geom_density(fill="#C9492C", col="black", alpha=0.8) +
  scale_x_continuous(name = "Distribution of Sample Means", limits=c(-4,4)) +
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



X <- rnorm(100000, mean=0, sd=0.5)
X2 <- rnorm(100000, mean=1.25, sd=0.5)

sum(X2>=0.825)/100000

x <- sample(X, 100, replace = FALSE)

ggplot(mapping = aes(x = X2)) +
  geom_density(fill="#43ADE2", col="black", alpha=0.7) +
  geom_density(aes(x=X), fill="#C9492C", col="black", alpha=0.7) +
  scale_x_continuous(name = "Distribution of Sample Means", limits=c(-2,4)) +
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

mean(x)
sd(x)


# We will create an empty dataframe to store our output in...
size = 100
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
  geom_histogram(fill="#b2df8a", col="black", alpha=0.8) +
  scale_x_continuous(name = "Sample Means", limits=c(-0.5,0.5)) +
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
