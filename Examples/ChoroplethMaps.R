#########################################################################################################
# SPL Project
# 
#########################################################################################################
# Choropleth Map Small Multiples
# Inspiration: https://stackoverflow.com/questions/9186529/grid-with-choropleth-maps-in-ggplot2
# https://ryouready.wordpress.com/2009/11/16/infomaps-using-r-visualizing-german-unemployment-rates-by-color-on-a-map/
# 
#########################################################################################################

# SET WORKSPACE
rm(list = ls())

wd <- file.path(Sys.getenv("HOME"), "Documents/Projects/SPL_Project") # if PC, use "USERPROFILE" instead of "HOME"
setwd(wd)

# LOAD PACKAGES
needs(viridis, ggplot, dplyr)

