library(car); library(tidyverse)

# Independent Samples T.Test ----
# Equal Variances Assumed
t.test(y~Xb1, data=DATAFRAME, var.equal=TRUE, paired=FALSE)
# Or
mod1<-lm(y~Xb1, data=DATAFRAME)

# Equal Variances NOT Assumed
# Implements Welch's correction to degrees of freedom
t.test(y~Xb1, data=DATAFRAME, var.equal=FALSE, paired=FALSE)


# Paired Sample T.Test ----
t.test(y~Xw1, data=DATAFRAME, paired=TRUE)
# To run as regression would require a mixed-effect regression package
# like lme4:
mod1<-lmer(y~Xw1+(1|SubID), data=DATAFRAME, REML=FALSE)

# One-Way Analysis of Variance ----
mod1<-aov(y~Xb1, data=DATAFRAME)
Anova(mod1, type="III")


# One-Way RM ANOVA ----
mod1<-aov(Y ~ Xw1 + Error(SubID/Xw1), data=DATAFRAME)
Anova(mod1, type="III")

# Two Way ANOVA Between Subjects ----
mod1<-aov(Y ~ Xb1*Xb2, data=DATAFRAME)
Anova(mod1, type="III")

# Two Way RM ANOVA (Fully Within Subjects) ----
mod1 <- aov(Y ~ Xw1*Xw2 + Error(SubID/(Xw1*Xw2)), data=DATAFRAME)
Anova(mod1, type="III")

# Mixed Factorial ANOVA ----
mod1 <- aov(Y ~ Xb1*Xw1 + Error(SubID/Xw1), data=DATAFRAME)
Anova(mod1, type="III")





