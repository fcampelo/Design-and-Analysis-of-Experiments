# clear workspace
rm(list = ls())

# Read data
amount.coins <- read.csv("00_amount_coins.csv", header = FALSE)[, 1]
total.value  <- read.csv("00_total_value.csv", header = FALSE)[, 1]

# Check data
err1 <- which(amount.coins != round(amount.coins)) # detect non-integer amounts
err2 <- which(round(100 * total.value) %% 5 != 0)  # detect non-multiples of $.05

# Remove inconsistencies
if (length(err1)) amount.coins <- amount.coins[-err1]
if (length(err2)) total.value  <- total.value[-err2]

# Exploratory data analysis
library(car)
par(mfrow = c(4, 2))
plot(seq_along(total.value), total.value, 
     type = "p", pch = 16, cex = 1.5,
     main = "Guessed values, Total Value")
abline(reg = lm(total.value ~ seq_along(total.value)))

plot(seq_along(amount.coins), amount.coins, 
     type = "p", pch = 16, cex = 1.5,
     main = "Guessed values, Amount of Coins")
abline(reg = lm(amount.coins~seq_along(amount.coins)))

dotchart(total.value, pch = 16, 
         main = "Guessed values, Total Value")
dotchart(amount.coins, pch = 16, 
         main = "Guessed values, Amount of Coins")

plot(density((total.value)), lwd = 2,
     main = "Estimated PDF, Total Value")
plot(density((amount.coins)), lwd = 2,
     main = "Estimated PDF, Amount of Coins")

qqPlot(total.value, pch = 16, cex = 1.5,
       main = "QQ plot, Total Value")
qqPlot(amount.coins, pch = 16, cex = 1.5,
       main = "QQ plot, Amount of Coins")
par(mfrow = c(1, 1))

shapiro.test(total.value)  # <-- pretty OK. 
shapiro.test(amount.coins) # <-- pretty far from normal

# Check sequential dependence in Total Value (where it may have an effect)
summary(lm(total.value ~ seq_along(total.value)))
durbinWatsonTest(lm(total.value ~ 1))

# Get a confidence interval for the mean of Total Value (under normality assumption)
# (t.test provides it as a side effect, even if we're not actually testing any 
# hypotheses)
a <- t.test(total.value, conf.level = 0.95)
a$conf.int


# Get a confidence interval for the mean of Total Amount of Coins
# In this case (non-normality) we have some options. Here I'll illustrate the 
# bootstrap CI.

library(boot)
set.seed(12345)

# Bootstrap
my.boots <- boot(data      = amount.coins, 
                 statistic = function(x, i){mean(x[i])}, 
                 R         = 999)

par(mfrow = c(2, 1))
plot(density(my.boots$t), lwd = 2, 
     main = "Estimated sampling distribution of means")
qqPlot(my.boots$t, pch = 16, cex = 1.5,
       main = "QQ plot, bootstrap means")
par(mfrow = c(1, 1))

# Bootstrap CI
boot.ci(my.boots, conf = 0.95, type = "norm")

# Compare it against normal-based CI:
# (should be similar, since the sampling distribution of the means is quite 
# normal-ish)
b <- t.test(amount.coins, conf.level = 0.95)
b$conf.int
