# clean workspace
rm(list=ls())

# install required packages if needed
if (!("ggplot2" %in% rownames(installed.packages()))){
    install.packages("ggplot2")
}

if (!("lmtest" %in% rownames(installed.packages()))){
    install.packages("lmtest")
}
#===================




# png("../figs/labdata-resplot.png", width = 600, height = 300)
# par(mfrow=c(1,2))
# plot(seq_along(subset(data, Place=="Lab")[,2]),
#      subset(data, Place=="Lab")[,2],
#      ylim = range(pretty(data[,2])),
#      type = "b",
#      pch = 20,
#      main = "Laboratory",
#      ylab = "Hole Area",
#      xlab = "Observation order")
# grid(NA,NULL,lwd=2)
# plot(seq_along(subset(data, Place=="DepDef")[,2]),
#      subset(data, Place=="DepDef")[,2],
#      ylim = range(pretty(data[,2])),
#      type = "b",
#      pch = 20,
#      main = "Dept. Defence",
#      ylab = " ",
#      xlab = "Observation order")
# grid(NA,NULL,lwd=2)
# dev.off()