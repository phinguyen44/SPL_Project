#################################################################################################
# EDA-Corrplots.R
#
#################################################################################################
# Description:
# Corrplots of variables in data set
#
#################################################################################################

#################################################################################################
# LOAD DATA

rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/SPL_Project")
setwd(wd)

source("Scripts/EDA.R")
needs(PerformanceAnalytics, corrplot)

#################################################################################################
# Do corrplots

# just for complete cases to check, remove factors
df.slim.cc = df.slim[complete.cases(df.slim), ] %>% 
  select(which(sapply(df.slim, function(x) !is.factor(x))))

# IT DOESN'T LOOK PRETTY. Work on this.
chart.Correlation(df.slim.cc)
corrplot(cor(df.slim.cc), method = "number", type = "upper")
