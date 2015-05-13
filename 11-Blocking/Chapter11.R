# clean workspace
rm(list=ls())

# install required packages if needed
# packages_needed <- c()
# for (package_name in packages_needed) {      
#       if (!(package_name %in% rownames(installed.packages()))){
#             install.packages(package_name)
#       }
# }
#===================

# Load data
data <- read.table("../data files/algo.csv",
                   header = TRUE)