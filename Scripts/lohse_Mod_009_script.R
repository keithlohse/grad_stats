## Opening Libraries -----------------------------------------------------------
library("car"); library("tidyverse"); library("ez");

## Setting the Directory -------------------------------------------------------
setwd("YOUR DIRECTORY HERE")
# let's see what is in the Data folder
list.files("./Data")

DATA <- read.table("./Data/ex9.1.txt", header = TRUE, sep = "")
head(DATA)
DATA$subID <- factor(seq(1, 18, 1))

contrasts(DATA$Drug) <- contr.poly(3)
contrasts(DATA$Psychotherapy) <- contr.poly(2)


# Two-Way ANOVA as a general linear model:
mod1 <- lm(Mood~Drug*Psychotherapy, data=DATA)
Anova(mod1, type="III")
summary(mod1)

# Handling our Two-Way ANOVA using ezANOVA:
ezANOVA(
  data = DATA
  , wid = .(subID)
  , dv = .(Mood)
  , between = .(Drug, Psychotherapy)
  , type = 3
  , detailed = TRUE
  , return_aov = TRUE
)


# Extracting means and standard errors
MEANS <- DATA %>% group_by(Drug, Psychotherapy) %>%
  summarize(mean_mood = mean(Mood, na.rm=TRUE),
            stdev = sd(Mood, na.rm=TRUE),
            n = n())

as.data.frame(MEANS)

MEANS$se <- MEANS$stdev/sqrt(MEANS$n)

MEANS

ggplot(MEANS, aes(x = Drug, y = mean_mood)) +
  geom_bar(aes(fill=Psychotherapy), col="black", 
           stat="identity", width = 0.5, position=position_dodge())+
  geom_errorbar(aes(group = Psychotherapy, ymin = mean_mood-se, ymax=mean_mood+se),
                width = 0.2, position=position_dodge(width=0.5))+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  scale_y_continuous(name = "Mood Ratings", limits=c(0,40)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size=16),
        legend.position = "right")


# Post-Hoc Tests for the Effect of Psychotherapy
summary(DATA$Drug)

posthoc1 <- lm(Mood~Psychotherapy, data=DATA[DATA$Drug=="DrugA",])
Anova(posthoc1, type="III")
summary(posthoc1)

posthoc2 <- lm(Mood~Psychotherapy, data=DATA[DATA$Drug=="DrugB",])
Anova(posthoc2, type="III")
summary(posthoc2)

posthoc3 <- lm(Mood~Psychotherapy, data=DATA[DATA$Drug=="Placebo",])
Anova(posthoc3, type="III")
summary(posthoc3)

# Post-Hoc Tests of the Effect of Drug
contrasts(DATA$Drug)
posthoc4 <- lm(Mood~Drug, data=DATA[DATA$Psychotherapy=="Treatment",])
Anova(posthoc4, type="III")
summary(posthoc4)

posthoc5 <- lm(Mood~Drug, data=DATA[DATA$Psychotherapy=="Control",])
Anova(posthoc5, type="III")
summary(posthoc5)
