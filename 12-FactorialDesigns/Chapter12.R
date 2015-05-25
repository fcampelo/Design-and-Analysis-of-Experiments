# clean workspace
rm(list = ls())

# install required packages if needed
packages_needed <- c("ggplot2", "multicomp", "effects")
for (package_name in packages_needed) {      
      if (!(package_name %in% rownames(installed.packages()))){
            install.packages(package_name)
      }
}
#===================

# Load data
data <- read.table("../data files/motors.txt",
                   header = TRUE)

# Exploratory data analysis
library(ggplot2)
# png(filename = "../figs/motors_box1.png",
#     width = 1000, height = 400, 
#     bg = "transparent")
p <- ggplot(data, 
            aes(x    = Manufacturer,
                y    = Current.Amperes,
                fill = Manufacturer))
p + geom_boxplot() +
    facet_grid(.~State) + 
    theme(legend.position = "none",
          axis.text  = element_text(size = 16),
          axis.title = element_text(size = 18),
          strip.text = element_text(size = 18))

# dev.off()

# Fit anova model
model <- aov(Current.Amperes~State*Manufacturer,
             data = data)
summary(model)
summary.lm(model)$r.squared

# Graphical analysis of residuals
# png(filename = "../figs/motors_res.png",
#     width = 1000, height = 300, 
#     bg = "transparent")
par(mfrow = c(1,4))
plot(model, pch = 16, cex = 2)
par(mfrow = c(1,1))
# dev.off()

# Analytical tests
shapiro.test(model$residuals)
fligner.test(Current.Amperes ~ interaction(State, Manufacturer), 
             data = data)


# Multiple comparisons
# Discover number of effective replications (important if you want 
# to manually calculate the post-ANOVA t-tests)
replications(Current ~ State*Manufacturer,
             data = data)


library(multcomp)
# Comparison of level means for Manufacturer
mcp.manuf<-glht(model,
                linfct = mcp(Manufacturer = "Tukey"))

# png(filename = "../figs/motors_mcpManuf.png",
#     width = 500, height = 500, 
#     bg = "transparent")
plot(confint(mcp.manuf),
     cex.axis   = 1.2,
     cex        = 2)
# dev.off()


# Create meta-factor for interaction groups
interfac <- with(data, 
                 interaction(State, Manufacturer))

# Use group with the smallest sample mean as the reference
which.min(tapply(data$Current.Amperes, interfac, mean))
interfac <- relevel(interfac, ref = "Original.B")

# ReFit model
model2 <- aov(Current.Amperes ~ interfac,
              data = data)

# Multiple comparisons
mcp.inter <- glht(model2,
                linfct = mcp(interfac = "Dunnett"))

# png(filename = "../figs/motors_mcpDun.png",
#     width = 1000, height = 500, 
#     bg = "transparent")
par(mar = c(5,12,4,2))
plot(confint(mcp.inter), 
     xlim       = c(-0.2,1),
     cex.axis   = 1.2,
     cex        = 2)
# dev.off()


#===================

## Intraocular Lens Experiment

# Clear workspace
rm(list = ls())

# Load data
data <- read.table("../data files/lio.txt",
                   header = TRUE)
# Fit model
model<-aov(Conf.rate ~ .^3,
           data = data)
summary(model)

# Use Daniel's plot to detect meaningful effects
effect.est <- as.numeric(model$effects[-1]) # Use the "-1" to remove the 
                                            # first estimated value ( which
                                            # is the grand mean)
# png(filename = "../figs/lio_Daniel.png",
#     width = 600, height = 600, 
#     bg = "transparent")
qq.obj <- qqnorm(effect.est,
                 datax      = TRUE,
                 pch        = 16, 
                 cex        = 2,
                 cex.axis   = 2,
                 cex.main   = 2,
                 cex.lab    = 2,
                 ylim       = c(-0.8, 0.8),
                 las        = 1)
qqline(effect.est,
       datax = TRUE)

qq.text <- rownames(summary.aov(model)[[1]])
text(qq.obj$x,
     qq.obj$y,
     labels = qq.text,
     pos    = 4,
     offset = 0.5,
     cex    = 2)
# dev.off()

# Simplified model
model2 <- aov(Conf.rate ~ .,
              data = data)
summary(model2)
summary.lm(model2)$r.squared
shapiro.test(model2$residuals)

# Explore specific differences
library(multcomp)
summary(glht(model2, 
             linfct = mcp(CNCTime = "Tukey")))
summary(glht(model2, 
             linfct = mcp(PolTime = "Tukey")))

library(effects)
lio.effs <- allEffects(model2)

trellis.par.set(list(axis.text = list(cex = 3),
                     par.ylab.text = list(cex = 3),
                     par.xlab.text = list(cex = 3)))

# png(filename = "../figs/lio_effs.png",
#     width = 500, height = 500, 
#     bg = "transparent")
plot(lio.effs)
# dev.off()
