# clean workspace
# rm(list=ls())

#===================
# Calculate sample size for steel rods experiment
(ss.calc <- power.t.test(delta       = 15,
                         sd          = 15,
                         sig.level   = 0.05,
                         power       = 0.8,
                         type        = "two.sample",
                         alternative = "one.sided"))
ceiling(ss.calc$n)


# Read steelrods data from a tab-separated file, and attach it to the workspace
y <- read.table("../data files/steelrods.txt",
                header = TRUE)
print(y)

#===================
# Perform two-sample t-test to compare means 
# (assuming equal but unknown variances)
t.test(y$Length.error ~ y$Process, 
       alternative = "less", 
       mu          = 0, 
       var.equal   = TRUE, 
       conf.level  = 0.95)

#===================
# Verify the assumptions: 
#   normality, equality of variances, independence of residuals.

## Normality
### QQ-plot for each group

# pdf("../figs/steelrodsqq.pdf",
#     width=5,
#     height=10) # comment to open plot in R

library(car)
qqPlot(y$Length.error, 
       groups = y$Process, 
       cex    = 1.5, 
       pch    = 16,
       layout = c(2, 1),
       las    = 1)

# dev.off() # comment this if you commented the pdf command.

### Shapiro-Wilk test for each group
shapiro.test(y$Length.error[y$Process == "new"])
shapiro.test(y$Length.error[y$Process == "old"])


# Equality of variances: Fligner-Killeen test
fligner.test(Length.error ~ Process, data = y)

# pdf("../figs/steelrodsvar.pdf",
#     width=5,
#     height=5) # comment to open plot in R

resid <- tapply(X     = y$Length.error, 
                INDEX = y$Process, 
                FUN   = function(x){x - mean(x)})
stripchart(x = resid, 
           vertical = TRUE, 
           pch = 16, 
           cex = 1.5, 
           las = 1, 
           xlab = "mean",
           ylab = "residuals")

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