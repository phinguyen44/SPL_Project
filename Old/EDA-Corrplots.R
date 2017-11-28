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

#################################################################################################
# Scatterplots, covariates vs. CASP score

# Reorganize data frames in a tidy set for plotting
base.tidy   = df.base.cc %>% 
  gather("xvar", "x", 1:(ncol(df.base.cc)-1))
fam.tidy    = df.fam.cc %>% 
  gather("xvar", "x", 1:(ncol(df.fam.cc)-1))
health.tidy = df.health.cc %>% 
  gather("xvar", "x", 1:(ncol(df.health.cc)-1))
job.tidy    = df.job.cc %>% 
  gather("xvar", "x", 1:(ncol(df.job.cc)-1))

# Draw basic scatterplots
p1 <- ggplot(data = base.tidy, aes(x = x, y = casp)) +
  geom_point(alpha = 0.10) +
  facet_grid(. ~ xvar, scales = "free")
p1

p2 <- ggplot(data = fam.tidy, aes(x = x, y = casp)) +
  geom_point(alpha = 0.10) +
  facet_grid(. ~ xvar, scales = "free")
p2

p3 <- ggplot(data = health.tidy, aes(x = x, y = casp)) +
  geom_point(alpha = 0.10) +
  facet_grid(. ~ xvar, scales = "free")
p3

p4 <- ggplot(data = job.tidy, aes(x = x, y = casp)) +
  geom_point(alpha = 0.10) +
  facet_grid(. ~ xvar, scales = "free")
p4

# TODO: Consider better plots for binary variables