# clean workspace
rm(list=ls())

#===================
# Greenpeas example
# Install required package

# load data (single vector)
sample <- scan("../data files/greenpeas.txt")

# Mean of normal distribution, variance unknown, one-sided test
t.test(sample,
       alternative = "less",
       mu          = 50,
       conf.level  = 0.99)


# Mean of normal distribution, variance unknown, two-sided test
t.test(sample,
       mu         = 50,
       conf.level = 0.99)


# Power for the 10-observation sample
power.t.test(n           = 10, 
             delta       = 0.5, 
             sd          = 1, 
             sig.level   = 0.01, 
             type        = "one.sample", 
             alternative = "one.sided")

# Minimal sample size for power = 0.85
power.t.test(power       = 0.85, 
             delta       = 0.5, 
             sd          = 1, 
             sig.level   = 0.01, 
             type        = "one.sample", 
             alternative = "one.sided")


# Validation of the normality assumption
# QQ plot

# pdf("../figs/GraphNorm.pdf", width = 5, height = 5) # <-- uncomment to save plot

library(car) # <--- Install if needed with install.packages("car")
qqPlot(sample,
       pch = 16,
       cex = 1.5,
       las = 1)

# dev.off() # <-- uncomment this if you uncommented the pdf() command above


# Shapiro-Wilk test of normality
shapiro.test(sample)


# Durbin-Watson test of independence (serial autocorrelations)
library(car)
durbinWatsonTest(lm(sample ~ 1))
