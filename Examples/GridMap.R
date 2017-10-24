#########################################################################################################
# SPL Project
# 
#########################################################################################################
# Europe Grid Map
# 
# Inspiration: https://policyviz.com/2017/05/04/european-tile-grid-map/
# Advanced: https://policyviz.com/2016/05/19/small-multiple-tile-grid-map/
# Graphical representation of Europe using square grids
#########################################################################################################

# SET WORKSPACE
rm(list = ls())

wd <- file.path(Sys.getenv("HOME"), "Documents/Projects/SPL_Project") # if PC, use "USERPROFILE" instead of "HOME"
setwd(wd)

# LOAD PACKAGES
needs(countrycode, ggplot2, dplyr)

# LOAD DATA
coordinates <- readxl::read_excel("Examples/Coordinates.xlsx", sheet = 1)
codes       <- countrycode_data

#########################################################################################################
# Data Preparation

df <- left_join(coordinates, codes, by = c("Country" = "country.name.en")) %>% select(Country, iso3c, X, Y, InSet)

colorIn <- c("TRUE" = "#253494", "FALSE" = "#cccccc")

#########################################################################################################
# Graph

p <- ggplot(data = df, aes(x = X, y = Y)) +
  geom_tile(aes(fill = InSet), color = "white", size = 0.6) + 
  geom_text(aes(label = iso3c), color = "white") + 
  
  scale_fill_manual(values = colorIn) + 
  
  labs(title = "EUROPE !!!!") + 
  labs(subtitle = "Countries in data set are colored navy") + 
  
  coord_fixed(ratio = 1) + 
  
  theme_minimal() + 
  theme(axis.line = element_blank()) + 
  theme(axis.text = element_blank()) +
  theme(axis.title = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") + 
  
  theme(plot.title = element_text(size=16)) +
  theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
p

ggsave("Examples/GridMap.png", plot = last_plot(), width = 6, units = "in")