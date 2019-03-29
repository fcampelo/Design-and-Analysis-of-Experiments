# clean workspace
rm(list=ls())

#===================
# Greenpeas example

# load data (single vector)
my.sample <- read.table("../data files/algorithm.txt")
my.sample <- as.numeric(my.sample$V1)

# Mean of normal distribution, variance unknown, one-sided test
t.test(my.sample,
       alternative = "greater",
       mu          = 50,
       conf.level  = 0.99)


# Mean of normal distribution, variance unknown, two-sided test
t.test(my.sample,
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
car::qqPlot(my.sample,
            pch = 16,
            cex = 1.5,
            las = 1)

# dev.off() # <-- uncomment this if you uncommented the pdf() command above


# Shapiro-Wilk test of normality
shapiro.test(my.sample)

# # bootstrapped distribution of sample means
# my.boot <- numeric(999)
# for (i in seq(my.boot)){
#   my.boot[i] <- mean(sample(my.sample, replace = TRUE))
# }
# par(mfrow = c(1, 2))
# hist(my.boot, breaks = 25, main = "Bootstrap Distribution of mean(x)")
# car::qqPlot(my.boot,
#             pch = 16,
#             cex = 1.5,
#             las = 1)
# par(mfrow = c(1, 1))

# Durbin-Watson test of independence (serial autocorrelations)
car::durbinWatsonTest(lm(my.sample ~ 1))
