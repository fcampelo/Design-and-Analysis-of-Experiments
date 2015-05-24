# clean workspace
rm(list=ls())

# install required packages if needed
packages_needed <- c("ggplot2", "multicomp")
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


# Comparison of interaction groups
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
plot(confint(mcp.inter), 
     xlim = c(-0.2,1))
