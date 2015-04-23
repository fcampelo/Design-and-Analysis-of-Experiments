# clean workspace
rm(list=ls())

# install required packages if needed
if (!("ggplot2" %in% rownames(installed.packages()))){
    install.packages("ggplot2")
}

if (!("lmtest" %in% rownames(installed.packages()))){
    install.packages("lmtest")
}
#===================

# Example 1: 2-sample equivalence of means

# load functions to calculate sample size for TOST
source("calcN_tost.R")

# Calculate sample size
calcN_tost2(alpha = 0.01,
            beta = 0.1,
            diff_mu = 0.5,
            tolmargin = 4,
            s1 = 5,
            s2 = 10)

# After collecting the data, proceed with analysis:
data<-read.table("../data files/labdata-example.csv",
                 header=T,
                 sep=",")
#png("../figs/labdata.png", width = 600, height = 300)
library(ggplot2)
ggplot(data) + 
    geom_boxplot(mapping = aes(x=Place,
                               y=HoleArea,
                               fill=Place))
#dev.off()

# Two one-sided t-tests
t.test(HoleArea~Place, 
       data = data, 
       alternative = "less", 
       mu = 4,
       conf.level = 0.99)$p.value
t.test(HoleArea~Place, 
       data = data, 
       alternative = "greater", 
       mu = -4,
       conf.level = 0.99)$p.value

# Get (1-2*alpha) CI and verify that it is indeed contained within (-delta,delta)
t.test(HoleArea~Place, 
       data = data, 
       conf.level = 0.98)$conf.int


# Check test assumptions
# Normality
#png("../figs/labdata-qqplots.png", width = 600, height = 300)
library(car)
par(mfrow=c(1,2))
qqPlot(subset(data,
              Place=="Lab")[,2],
       pch=20,
       main = "Laboratory",
       ylab = "Observed quantiles")
qqPlot(subset(data,
              Place=="DepDef")[,2],
       pch=20,
       main = "Dept. Defence",
       ylab = " ")
#dev.off()

# Independence
library(lmtest)
dwtest(HoleArea~Place, data=data)

#png("../figs/labdata-resplot.png", width = 600, height = 300)
par(mfrow=c(1,2))
plot(seq_along(subset(data, Place=="Lab")[,2]),
     subset(data, Place=="Lab")[,2],
     ylim = range(pretty(data[,2])),
     type = "b",
     pch = 20,
     main = "Laboratory",
     ylab = "Hole Area",
     xlab = "Observation order")
grid(NA,NULL,lwd=2)
plot(seq_along(subset(data, Place=="DepDef")[,2]),
     subset(data, Place=="DepDef")[,2],
     ylim = range(pretty(data[,2])),
     type = "b",
     pch = 20,
     main = "Dept. Defence",
     ylab = " ",
     xlab = "Observation order")
grid(NA,NULL,lwd=2)
#dev.off()