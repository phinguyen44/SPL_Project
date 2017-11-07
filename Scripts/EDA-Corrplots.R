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

# Complete cases only for the time being
df.base.cc   = filter(df.base, complete.cases(df.base))
df.fam.cc    = filter(df.fam, complete.cases(df.fam))
df.health.cc = filter(df.health, complete.cases(df.health))
df.job.cc    = filter(df.job, complete.cases(df.job))

# IT DOESN'T LOOK PRETTY. Work on this.
corrplot(cor(df.base.cc), 
         method      = "color", 
         type        = "upper",
         addCoef.col = "black", 
         tl.col      = "black", 
         tl.srt      = 45)

corrplot(cor(df.fam.cc), 
         method      = "color", 
         type        = "upper",
         addCoef.col = "black", 
         tl.col      = "black", 
         tl.srt      = 45)

corrplot(cor(df.health.cc), 
         method      = "color", 
         type        = "upper",
         addCoef.col = "black", 
         tl.col      = "black", 
         tl.srt      = 45)

corrplot(cor(df.job.cc), 
         method      = "color", 
         type        = "upper",
         addCoef.col = "black", 
         tl.col      = "black", 
         tl.srt      = 45)
