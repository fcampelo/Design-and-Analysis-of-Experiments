# clean workspace
rm(list=ls())

#===================
# Calculate sample size for steel rods experiment
(ss.calc <- power.t.test(delta       = 15,
                         sd          = 15,
                         sig.level   = 0.05,
                         power       = 0.8,
                         type        = "two.sample",
                         alternative = "two.sided"))
ceiling(ss.calc$n)


# Read steelrods data from a tab-separated file, and attach it to the workspace
y <- read.table("../data files/steelrods.txt",
                header = TRUE)
print(y)


#===================
# Example of doing the test step by step (to show some of R's syntax)

n <- 17   # We have 17 observations of each process

# Get sample means, standard deviations
means <- tapply(y$Length.error,
                y$Process,
                mean)

s <- tapply(y$Length.error,
            y$Process,
            sd)

# Calculate pooled standard deviation
sp <- sqrt(((n - 1) * s[1] ^ 2 + (n-1) * s[2] ^ 2) / (2 * n - 2))

# Test statistic:
t0 <- (means[1] - means[2]) / (sp * sqrt(2 / n))

# Critical values
tlims <- c(qt(.025, 32), qt(.975, 32))

print(tlims)
print(t0)

# p-value
p.value <- 2 * pt(t0, 2 * n - 2) # multiply by two since H1 is two-sided.
print(p.value)

#===================
# Much simpler way of doing this:
# Perform two-sample t-test to compare means 
# (assuming equal but unknown variances)
with(y,
     t.test(Length.error ~ Process, 
            alternative = "two.sided", 
            mu = 0, 
            var.equal = TRUE, 
            conf.level = 0.95))

#===================
# Verify the assumptions: 
#   normality, equality of variances, independence of residuals.

# Vector of residuals
# (invert the order of the means vector to get it in the right order
resid <- y$Length.error - rep(means[2:1],
                              each = n)

# Normality: Shapiro-Wilk test
shapiro.test(resid)

# pdf("../figs/steelrodsqq.pdf",
#     width=5,
#     height=5) # comment to open plot in R

library(car)
qqPlot(resid,
       pch = 16,
       cex = 1.5,
       las = 1)

# dev.off() # comment this if you commented the pdf command.


# Equality of variances: Fligner-Killeen test
with(y,
     fligner.test(Length.error ~ Process))

# pdf("../figs/steelrodsvar.pdf",
#     width=5,
#     height=5) # comment to open plot in R

plot(x = rep(means[2:1],
             each = n),
     y = resid,
     xlab = "mean",
     pch = 16,
     cex = 1.5,
     las = 1)

# dev.off() # comment this if you commented the pdf command.

# pdf("../figs/steelrodsind.pdf",
#     width=5,
#     height=5) # comment to open plot in R

plot(resid,
     pch = 16,
     cex = 1.5,
     type = "b",
     las = 1)

# dev.off() # comment this if you commented the pdf command.


# Perform two-sample t-test to compare means (without assuming equal variances)
with(y,
     t.test(Length.error ~ Process,
            alternative = "two.sided",
            mu = 0,
            var.equal = FALSE,
            conf.level = 0.95))