# clean workspace
rm(list=ls())

# install required packages if needed
packages_needed <- c("multcomp")
for (package_name in packages_needed) {      
      if (!(package_name %in% rownames(installed.packages()))){
            install.packages(package_name)
      }
}
#===================

## Paper strength experiment

# Experimental parameters
alpha     <- 0.1
beta      <- 0.2
delta     <- 5
sigma.hat <- 6

# Read data
paper   <- read.table(file = "../data files/paper_strength.csv", 
                      header = TRUE, 
                      sep = ",")
summary(paper)

# First look at the data
# png(filename = "../figs/paperbox.png",
#     width = 600, height = 600, 
#     bg = "transparent")
boxplot(TS_kPa~Hardwood, 
        data = paper, 
        xlab = "Hardwood",
        ylab = "TS (kPa)", 
        main = "Paper strength data",
        pch  = 16,
        col  = "gray")
# dev.off()

# Computational modeling and analysis
model <- aov(TS_kPa~Hardwood, 
             data = paper)
summary.aov(model)

# Check normality
shapiro.test(model$residuals)

library(car)
# png(filename = "../figs/paperqq.png",
#     width = 600, height = 600, 
#     bg = "transparent")
qqPlot(model$residuals, 
       pch = 16, 
       lwd = 3, 
       cex = 2, 
       las = 1)
# dev.off()

# Check homoscedasticity
fligner.test(TS_kPa~Hardwood, 
             data = paper)

# png(filename = "../figs/papervar.png",
#     width = 600, height = 600, 
#     bg = "transparent")
plot(x    = model$fitted.values,
     y    = model$residuals,
     cex  = 2,
     las  = 1,
     pch  = 16,
     xlab = "Fitted values",
     ylab = "Residuals")
grid(NULL,NULL, lwd=2, col = "#44444422")
# dev.off()

# Check independence
durbinWatsonTest(model)

# png(filename = "../figs/paperind.png",
#     width = 600, height = 600, 
#     bg = "transparent")
plot(x    = seq_along(model$residuals),
     y    = model$residuals,
     type = "l",
     las  = 1,
     lwd  = 2,
     lty  = 1,
     xlab = "Residual order",
     ylab = "Residual value")
points(x    = seq_along(model$residuals),
       y    = model$residuals,
       type = "p",
       cex  = 2,
       pch  = 16,
       col  = as.numeric(paper[, 1]))
grid(NA,NULL, lwd=2, col = "#44444422")
# dev.off()


## Multiple comparisons

# Situation 1: all vs. all
library(multcomp)
paper_tukey <- glht(model, 
                    linfct = mcp(Hardwood = "Tukey"))
paper_tukey_CI <- confint(paper_tukey, 
                          level = 0.95)

# png(filename = "../figs/papertukey.png",
#     width = 600, height = 600, 
#     bg = "transparent")
plot(paper_tukey_CI, 
     xlab       = "Tensile Strength (kPa)",
     sub        = "- Hardwood data -",
     cex.axis   = 1.2,
     cex        = 2)
# dev.off()


# Situation 2: all vs. B
paper$Hardwood  <- relevel(paper$Hardwood, 
                           ref = "B")
model2          <- aov(TS_kPa~Hardwood, 
                       data = paper)
paper_dunnett   <- glht(model2, 
                        linfct = mcp(Hardwood = "Dunnett"))
paper_dunnett_CI <- confint(paper_dunnett, 
                            level = 0.95)

# png(filename = "../figs/paperdunnett.png",
#     width = 600, height = 600, 
#     bg = "transparent")
plot(paper_dunnett_CI, 
     xlab       = "Tensile Strength (kPa)",
     sub        = "- Hardwood data -",
     cex.axis   = 1.2,
     cex        = 2)
# dev.off()

