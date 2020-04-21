# clean workspace
rm(list=ls())

# Uncomment to install required packages if needed
# packages_needed <- c("multcomp", "ggplot2", "car")
# for (package_name in packages_needed) {      
#       if (!(package_name %in% rownames(installed.packages()))){
#             install.packages(package_name)
#       }
# }
#===================

## Paper strength experiment

# Experimental parameters
alpha     <- 0.1
beta      <- 0.2
delta     <- 5
sigma.hat <- 6

# Read data
paper   <- read.table(file   = "../data files/paper_strength.csv", 
                      header = TRUE, 
                      sep    = ",")
summary(paper)

# First look at the data
# png(filename = "../figs/paperbox.png",
#     width = 600, height = 600,
#     bg = "transparent")

library(ggplot2)
ggplot(paper, 
       aes(x    = Fiber.type,
           y    = TS.kPa,
           fill = Fiber.type)) + 
  geom_boxplot() + geom_point() + 
  ggtitle("Paper Strenght Data",
          "(original data + boxplots)") + 
  theme(legend.position = "none")
# dev.off()


# Computational modeling and analysis
my.model <- aov(TS.kPa ~ Fiber.type, 
             data = paper)
summary.aov(my.model)

# Residuals
my.model$residuals


# Check normality
shapiro.test(my.model$residuals)

library(car)
# png(filename = "../figs/paperqq.png",
#     width = 600, height = 600, 
#     bg = "transparent")

qqPlot(my.model$residuals, 
       pch = 16, 
       lwd = 3, 
       cex = 2, 
       las = 1)
# dev.off()

# Check homoscedasticity
fligner.test(TS.kPa ~ Fiber.type, 
             data = paper)

# png(filename = "../figs/papervar.png",
#     width = 600, height = 600, 
#     bg = "transparent")
plot(x    = my.model$fitted.values,
     y    = my.model$residuals,
     cex  = 2,
     las  = 1,
     pch  = 16,
     xlab = "Fitted values",
     ylab = "Residuals")
grid(nx = NULL, ny = NULL, 
     lwd = 2, col = "#44444422")
# dev.off()

# Check serial independence (only really makes sense if data are presented 
# ordered by a possibly influential, unmodelled variable such as data collection 
# sequence, etc.)

# durbinWatsonTest(model)
# 
# # png(filename = "../figs/paperind.png",
# #     width = 600, height = 600, 
# #     bg = "transparent")
# plot(x    = seq_along(model$residuals),
#      y    = model$residuals,
#      type = "l",
#      las  = 1,
#      lwd  = 2,
#      lty  = 1,
#      xlab = "Residual order",
#      ylab = "Residual value")
# points(x    = seq_along(model$residuals),
#        y    = model$residuals,
#        type = "p",
#        cex  = 2,
#        pch  = 16,
#        col  = as.numeric(paper[, 1]))
# grid(NA,NULL, lwd=2, col = "#44444422")
# # dev.off()


## Multiple comparisons

# Situation 1: all vs. all
library(multcomp)
mc1    <- glht(my.model, 
               linfct = mcp(Fiber.type = "Tukey"))
mc1_CI <- confint(mc1, level = 0.95)

# png(filename = "../figs/papertukey.png",
#     width = 600, height = 600, 
#     bg = "transparent")
plot(mc1_CI, 
     xlab       = "Tensile Strength (kPa)",
     sub        = "- Hardwood data -",
     cex.axis   = 1.2,
     cex        = 2)
# dev.off()


# Situation 2: all vs. B
paper$Fiber.type <- relevel(paper$Fiber.type, ref = "B")
model2           <- aov(TS.kPa ~ Fiber.type, data = paper)
mc2              <- glht(model2, linfct = mcp(Fiber.type = "Dunnett"))
mc2_CI           <- confint(mc2, level = 0.95)

# png(filename = "../figs/paperdunnett.png",
#     width = 600, height = 600, 
#     bg = "transparent")
plot(mc2_CI, 
     xlab       = "Tensile Strength (kPa)",
     sub        = "- Hardwood data -",
     cex.axis   = 1.2,
     cex        = 2)
# dev.off()

#==========

## Sample size calculations for anova
a       <- 4
alpha   <- 0.05
sigma   <- 7
delta   <- 12
beta    <- 0.2

# Case 1: two levels symmetrically biased about the grand mean
tau <- c(-delta / 2, 
         delta / 2, 
         rep(0, a - 2)) # define tau vector
n   <- 2        # initial n

while (qf(p = 1 - alpha, df1 = a - 1, df2 = a * (n - 1)) > 
       qf(p = beta, df1 = a - 1, a * (n - 1), df2 = n * sum(tau ^ 2) / sigma ^ 2)){
  n <- n + 1
}

# Using power.anova.test():
vartau <- var(tau)
power.anova.test(groups      = 4, 
                 between.var = vartau, 
                 within.var  = sigma ^ 2, 
                 sig.level   = alpha, 
                 power       = 1 - beta)$n


# Case 2: one levels biased relative to the others
tau <- c(-delta * (a - 1) / a, 
         rep(delta / a, a - 1)) # define tau vector
vartau <- var(tau)
power.anova.test(groups      = 4, 
                 between.var = vartau, 
                 within.var  = sigma^2, 
                 sig.level   = alpha, 
                 power       = 1 - beta)$n
