## Opening Libraries -----------------------------------------------------------
library("car"); library("tidyverse"); library("ez");

## Setting the Directory -------------------------------------------------------
setwd("YOUR DIRECTORY HERE")
# let's see what is in the Data folder
list.files("./Data")

## Opening an example from the textbook ----------------------------------------
dat <- read.csv("./Data/data_ALTITUDE.csv", header = TRUE, sep = ",")
head(dat)

dat <- dat[order(dat$altitude),] 
head(dat)

dat$alt_km<-dat$altitude/1000

# ------------------------- Moderated Models -----------------------------------
plot(decrease~alt_km, pch=21, bg="blue", cex=1, data=dat,
     ylab="Decrease in VO2max", xlab="Atitude (km)") 

plot(decrease~BVO2, pch=21, bg="blue", cex=1, data=dat,
     ylab="Decrease in VO2max", xlab="Baseline VO2max") 


mod0<-lm(decrease~alt_km+BVO2, data=dat)
summary(mod0)

mod1<-lm(decrease~alt_km*BVO2, data=dat)
summary(mod1)


# Mean Centering One Variable:
dat$BVO2.c<-dat$BVO2-mean(dat$BVO2)
mean(dat$BVO2)
head(dat)

mod2<-lm(decrease~alt_km*BVO2.c, data=dat)
summary(mod2)

mean(dat$BVO2)

ggplot(data = dat, 
       mapping = aes(x = alt_km, y = decrease)) +
  geom_point(aes(fill=BVO2), pch=21, size=2, stroke=1, col="black", alpha = .8) +
  scale_x_continuous(name = "Altitude (km)") +
  scale_y_continuous(name = "Decrease in VO2max")+
  scale_fill_gradient(low="red", high="blue")+
  theme_bw()+
  theme(axis.text=element_text(size=16, colour="black"), 
        axis.title=element_text(size=16,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "right")






#------------------------- Nonlinear Models ------------------------------------
##############################
plot(decrease~alt_km, pch=21, bg="blue", cex=1, data=dat,
     ylab="Decrease in VO2max", xlab="Atitude (km)") 


mod1<-lm(decrease~alt_km, data=dat)
summary(mod1)
abline(mod1, col="red", lty=2, lwd=2)


###########################
dat$alt_km_sq<-dat$alt_km^2

plot(decrease~alt_km, pch=21, bg="blue", cex=1, data=dat,
     ylab="Decrease in VO2max", xlab="Atitude (km)", main="Quadratic Model") 


mod2<-lm(decrease~alt_km+alt_km_sq, data=dat)
summary(mod2)
dat$sq_pred<-fitted(mod2)
lines(y=dat$sq_pred, x=dat$alt_km, col="red", lwd=2, lty=2)

Anova(mod2, type='III')

## Mean centering altitude
dat$alt_km.c<-dat$alt_km-mean(dat$alt_km)
mean(dat$alt_km)

dat$alt_km.c_sq<-dat$alt_km.c^2
dat$alt_km.c_cu<-dat$alt_km.c^3

summary(dat$alt_km.c)
summary(dat$alt_km.c_sq)
summary(dat$alt_km.c_cu)

mean(dat$decrease)

mod3<-lm(decrease~alt_km.c+alt_km.c_sq+alt_km.c_cu, data=dat)
summary(mod3)

Anova(mod3, type="III")

plot(decrease~alt_km, pch=21, bg="blue", cex=1, data=dat,
     ylab="Decrease in VO2max", xlab="Atitude (km)", main="Cubic Model") 

dat$cu_pred<-fitted(mod3)
lines(y=dat$cu_pred, x=dat$alt_km, col="black", lwd=2, lty=2)


  
  
mod4<-lm(decrease~alt_km.c+alt_km.c_sq, data=dat)
summary(mod4)

Anova(mod4, type="III")
