# Uniform Population of Dice Rolls
set.seed(1)
pop<-runif(n=100000, min=1, max=20)
hist(pop)
summary(pop)


index<-c(1:10000) # Set the upper limit to the number of experiments you want to run

DATA<-data.frame(index) #We will create an empty dataframe to store our output in...
DATA$mean10<-NULL

for (i in 1:length(DATA$index)) {
  s1 <- sample(pop, 10, replace = FALSE, prob = NULL)

  DATA$mean10[i]<-mean(s1)
}

head(DATA)


hist(DATA$mean10, col="grey40", main="Distribution of Sample Means, N = 10",
     xlab="Mean of 10 Dice Rolls")
means<-c(10.6,9.3,11.3,7.4,11.8,12.8,10.4,11,13.3)
abline(v=means, col=c("green","red","blue","orange","gold","white","cornsilk",
       "purple","black"), lwd=2)

mean(DATA$mean10)
sd(DATA$mean10)


Qobs<-(means-10.51)/sd(DATA$mean10)
Qobs

# Approximating with a Normal Distribution
xfit<-seq(from=-3, to=3, by=0.1)
normfit<-dnorm(xfit, mean=0, sd=1)
par(bg="gray80")
plot(x=xfit, y=normfit, col="black", lwd=2, xlab="(x-mu)/SE", ylab="Density",
     main="Assuming Normal Distribution of Sample Means (Incorrect)", ylim=c(0,0.5))
abline(v=Qobs, col=c("green","red","blue","orange","gold","white","cornsilk",
                      "purple","black"), lwd=2)

pnorm(Qobs, mean=0, sd=1, lower.tail=FALSE)

# Approximating with a T-Distribution
xfit<-seq(from=-3, to=3, by=0.1)
tfit<-dt(xfit, df=9)
par(bg="gray80")
plot(x=xfit, y=tfit, col="black", lwd=2, xlab="(x-mu)/SE", ylab="Density",
     main="Assuming t-Distribution of Sample Means (Correct)", ylim=c(0,0.5))
abline(v=Qobs, col=c("green","red","blue","orange","gold","white","cornsilk",
                      "purple","black"), lwd=2)
pt(Qobs,(10-9),lower.tail=FALSE)






