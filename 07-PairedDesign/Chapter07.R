# clean workspace
rm(list=ls())

# install required packages if needed
if (!("lmtest" %in% rownames(installed.packages()))){
    install.packages("lmtest")
}
#===================

## Paired design (across-problems comparison of algorithms)

# Experimental design
delta_min <- 10
desired_beta <- 0.8
alpha <- 0.05

# Read data
data<-read.table("../data files/soltimes.csv",
                 header=T)
summary(data)

# "Problem" is a categorical variable, not a continuous one:
data$Problem<-as.factor(data$Problem)
summary(data)

# Summarize the n=30 repeated measures on each Problem:Algorithm combination by
# their mean value
aggdata<-aggregate(Time~Problem:Algorithm,
                   data=data,
                   FUN=mean)
print(aggdata)

# Perform paired t-test
t.test(Time~Algorithm,
       paired=T,
       data=aggdata)

# The test above is similar to:
difTimes<-with(aggdata,
            Time[1:7]-Time[8:14])
t.test(difTimes)

# Check for deviations of normality
# 1: Normal QQplot
library(car)
pdf("../figs/soltimesqq.pdf",
    width=5,
    height=5) # comment to open plot in R

qqPlot(difTimes,
       pch=16,
       cex=1.5,
       las=1)

# Highlight the observed outlier
indx<-which(difTimes==max(difTimes))
pt<-qqnorm(difTimes,plot.it=F)
points(pt$x[indx],pt$y[indx],pch=1,cex=2,col=2)

dev.off() # comment this if you commented the pdf command.

# 2. Shapiro-Wilk test
shapiro.test(difTimes)

# Verify robustness of the conclusion to the removal of the outlier
t.test(difTimes[-indx])$p.value
t.test(difTimes[-indx])$conf.int


# What happens if we fail to consider the problem effects?
t.test(Time~Algorithm,data=aggdata)
