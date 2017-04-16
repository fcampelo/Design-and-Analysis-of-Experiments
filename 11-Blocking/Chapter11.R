# clean workspace
rm(list=ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# install required packages if needed
packages_needed <- c("stringr","ggplot2", "multcomp")
for (package_name in packages_needed) {      
      if (!(package_name %in% rownames(installed.packages()))){
            install.packages(package_name)
      }
}
#===================

# Load data
data <- read.table("../data files/algo.csv",
                   header = TRUE)

# Aggregate data (algorithm means by instance group)
aggdata <- with(data,
                aggregate(x   = Result,
                          by  = list(Algorithm, Group),
                          FUN = mean))

# Rename columns
names(aggdata) <- c("Algorithm", 
                    "Instance_Group",
                    "Y")

# Coerce categorical variables to factors
for (i in 1:2){
      aggdata[, i] <- as.factor(aggdata[, i])
}

# Make factor level names more informative
levels(aggdata$Algorithm) <- c("Original",
                               unlist(lapply("Mod",
                                             paste0,
                                             1:6)))

summary(aggdata)

# Exploratory data analysis: plot observations by Algorithm and Instance_Group
library(ggplot2)

# png(filename = "../figs/algo_lineplot.png",
#     width = 1000, height = 400, 
#     bg = "transparent")
p <- ggplot(aggdata, aes(x = Instance_Group, 
                         y = Y, 
                         group = Algorithm, 
                         colour = Algorithm))
p + geom_line(linetype=2) + geom_point(size=5)
# dev.off()


# Statistical modeling
# First model
model <- aov(Y~Algorithm+Instance_Group,
             data = aggdata)
summary(model)
summary.lm(model)$r.squared

# Graphical test of assumptions
# png(filename = "../figs/algo_res1.png",
#     width = 1000, height = 500, 
#     bg = "transparent")
par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)
# dev.off()


# Try log-transformed data
model2 <- aov(log(Y)~Algorithm+Instance_Group,
              data = aggdata)
summary(model2)
summary.lm(model2)$r.squared

# Graphical test of assumptions
# png(filename = "../figs/algo_res2.png",
#     width = 1000, height = 500, 
#     bg = "transparent")
par(mfrow = c(2, 2))
plot(model2, pch = 20, las = 1)
# dev.off()

library(car)
# png(filename = "../figs/algo_qq.png",
#     width = 600, height = 600, 
#     bg = "transparent")
par(mfrow = c(1, 1))
qqPlot(model2$residuals, pch = 20, las = 1)
# dev.off()


# Blocking efficiency
mydf        <- as.data.frame(summary(model2)[[1]])
MSblocks    <- mydf["Instance_Group","Mean Sq"]
MSe         <- mydf["Residuals","Mean Sq"]
a           <- length(unique(aggdata$Algorithm))
b           <- length(unique(aggdata$Instance_Group))
((b - 1) * MSblocks + b * (a - 1) * MSe) / ((a * b - 1) * MSe)

# Post-hoc comparisons
library(multcomp)
duntest     <- glht(model2,
                    linfct = mcp(Algorithm = "Dunnett"))

duntestCI   <- confint(duntest)

# png(filename = "../figs/algo_mcp.png",
#     width = 1000, height = 500, 
#     bg = "transparent")
par(mar = c(5, 8, 4, 2), las = 1)
plot(duntestCI,
     xlab = "Mean difference (log scale)")
# dev.off()
