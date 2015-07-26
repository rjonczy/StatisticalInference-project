library(datasets)
data(ToothGrowth)

str(ToothGrowth)
summary(ToothGrowth)

head(ToothGrowth)
tail(ToothGrowth)

# ---------------------------------------------------------
require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")
# ---------------------------------------------------------
g <- ggplot(subset(ToothGrowth, supp %in% c("VC", "OJ"))) 
g <- g + geom_boxplot(aes( x = dose, y = len, group = dose, colour = dose, fill = dose))
g <- g + facet_grid(. ~ supp)
g <- g + ggtitle("Analysis of Tooth Growth by Increasing Dose & Supp Type")
g <- g + theme(plot.title = element_text(lineheight=.40, face="bold"))
g
# ---------------------------------------------------------
library(ggplot2)
g <- ggplot(ToothGrowth, aes(x = factor(dose), y=len))
g <- g + geom_boxplot()
g <- g + facet_grid(~supp)
g <- g + ggtitle('Analyzing ToothGrowth data')
g
# ---------------------------------------------------------
library(plyr)
TG_summary <- ddply(ToothGrowth , .(dose, supp) , summarize, mean = mean(len), sd = sd(len))
as.factor(TG_summary$dose)
TG_summary

# ---------------------------------------------------------



# by supp
t.test(len ~ supp, data = ToothGrowth)

# by dose
tw05 <- subset(ToothGrowth, dose == 0.5)
tw10 <- subset(ToothGrowth, dose == 1.0)
tw20 <- subset(ToothGrowth, dose == 2.0)

t.test(len ~ supp, data = tw05, paired = FALSE)
t.test(len ~ supp, data = tw10, paired = FALSE)
t.test(len ~ supp, data = tw20, paired = FALSE)


# conclusion
