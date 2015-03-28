# clean workspace
rm(list=ls())

# install required packages if needed
if (!("lmtest" %in% rownames(installed.packages()))){
    install.packages("lmtest")
}
#===================

# Paired design (across-problems comparison of algorithms)

delta_min<-5
# aggregate(Time~Problem:Algorithm,data=x,FUN=mean)
# with(sumt,t.test(Time[1:7],Time[8:14],paired=T))