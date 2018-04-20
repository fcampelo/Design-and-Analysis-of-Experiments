# clean workspace
rm(list=ls())
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# install required packages if needed
packages_needed <- c("ggplot2","lmtest","dplyr")
for (package_name in packages_needed) {      
      if (!(package_name %in% rownames(installed.packages()))){
            install.packages(package_name)
      }
}
#===================

## Generate initial figures
### Load data
library(dplyr)
data<-tbl_df(read.table("../data files//example_algodata.csv",
                        sep=",",
                        header=TRUE))

### Precondition data using dplyr
data <- data %>%
      mutate(Algorithm = as.factor(Algorithm),
             Instance_Group = as.factor(Instance_Group))

# Get residuals 
res <- as.numeric(lm(Mean_Performance~Algorithm+Instance_Group, 
                     data=data)$residual)

### Generate figure
# png("../figs/logn_qq.png", width = 600, height = 600)
qqdata<-with(qqnorm(res),
             data.frame(x=sort(x),
                        y=y[order(x)]))
library(car)
par(las=1)
qqPlot(res,
       pch = 16,
       cex = 2,
       ylab = "Data",
       xlab = "Normal Quantiles")
points(qqdata$x,
       qqdata$y,
       type = "l",
       lty = 3,
       lwd=3)
# dev.off()

# Another figure
# png("../figs/expdata.png", width = 600, height = 600)
set.seed(1234)
expdata <- data.frame(x = rexp(1000))
library(ggplot2)
ggplot(expdata, aes(x=x))+
      geom_histogram(aes(y=..density..),binwidth=.05)+
      geom_density(col="blue",lwd=2)
# dev.off()

#===================

