# clean workspace
rm(list=ls())

# install required packages if needed
if (!("lmtest" %in% rownames(installed.packages()))){
    install.packages("lmtest")
}
#===================

