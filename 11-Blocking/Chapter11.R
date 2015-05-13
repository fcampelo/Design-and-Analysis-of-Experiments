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
                   header = TRUE,
                   colClasses = c(rep("factor", 4),
                                  "numeric"))

# Aggregate data (algorithm means by instance group)
aggdata <- with(data,
                aggregate(x   = Result,
                          by  = list(Algorithm, Instance),
                          FUN = mean))
names(aggdata) <- c("Algorithm", 
                    "Instance_Group",
                    "Y")

# 