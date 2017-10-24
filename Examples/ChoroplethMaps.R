#########################################################################################################
# SPL Project
# 
#########################################################################################################
# Choropleth Map Small Multiples
# Inspiration: https://stackoverflow.com/questions/9186529/grid-with-choropleth-maps-in-ggplot2
# https://ryouready.wordpress.com/2009/11/16/infomaps-using-r-visualizing-german-unemployment-rates-by-color-on-a-map/
# https://gist.github.com/stared/fbca436c885c430a314a
# 
# other ideas - ggjoy, distributions over time
# 
#########################################################################################################

# SET WORKSPACE
rm(list = ls())

wd <- file.path(Sys.getenv("HOME"), "Documents/Projects/SPL_Project") # if PC, use "USERPROFILE" instead of "HOME"
setwd(wd)

# LOAD PACKAGES
needs(countrycode, maptools, mapproj, RColorBrewer, rgeos, ggplot2, dplyr, tidyr)

# LOAD DATA
coordinates <- readxl::read_excel("Examples/Coordinates.xlsx", sheet = 1)
codes       <- countrycode_data

#########################################################################################################
# Data Preparation

df <- left_join(coordinates, codes, by = c("Country" = "country.name.en")) %>% 
  filter(InSet == TRUE) %>% 
  select(eurostat) %>% 
  rename("CountryCode" = eurostat)

# Generate fake data
dummy <- replicate(6, sample(1:10, nrow(df), replace = TRUE))

df[, 2:(1+ncol(dummy))] <- as.character(dummy)
names(df)[2:ncol(df)]   <- seq(1990, 2015, length.out = 6)

# dat <- easySHARE_rel6_0_0
# # Let's just get data by wave, country, employment status, then summarize
# dat <- dat %>% 
#   select(mergeid, wave, country_mod, ep005_) %>%
#   filter(ep005_ > 0) %>% 
#   group_by(wave, country_mod) %>% 
#   summarize(unemployed = sum(ep005_ == 3)/sum(ep005_ == 2 | ep005_ == 3))

# NOTE: lots of missing data. tons of cleaning will be done

#########################################################################################################
# Maps

# get shape file (http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts#nuts13)
eurMap   <- readShapeSpatial("Examples/NUTS_2013_01M_SH/data/NUTS_RG_01M_2013")
eurMap   <- subset(eurMap, nchar(as.character(NUTS_ID)) == 2) # only country level
eurMapDf <- fortify(eurMap, region='NUTS_ID')

# note no israel data

# limit data to main Europe
eurMapDf <- subset(eurMapDf, long > -12 & long < 32 & lat > 34 & lat < 75)

# merge with dummy data
eurMapDf <- left_join(eurMapDf, df, by = c("id" = "CountryCode"))
eurMapDf <- eurMapDf[order(eurMapDf$order), ]

# tidy set to split by year
eurData <- gather(eurMapDf, "Year", "Rank", 8:13)

# draw plot (one)
p2 <- ggplot(data = eurMapDf) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=eurMapDf$'1990')) + 
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.1) +

  scale_fill_brewer(palette = "YlOrRd") + 
  
  theme_minimal() +
  theme(axis.line = element_blank()) + 
  theme(axis.text = element_blank()) +
  theme(axis.title = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(panel.grid = element_blank()) + 
  theme(legend.text = element_text(size = 8)) +
  theme(legend.position = "bottom") + 

  coord_map()
p2

# dumb hack
eurData$Rank <- as.numeric(eurData$Rank)
# draw plot (multiple) # WARNING: TAKES SO LONG TO LOAD
p <- ggplot(data = eurData) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=Rank)) + 
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.1) +
  
  scale_fill_gradient2(low = "white", high = "red") +
  
  theme_minimal() +
  theme(axis.line = element_blank()) + 
  theme(axis.text = element_blank()) +
  theme(axis.title = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(panel.grid = element_blank()) + 
  theme(legend.text = element_text(size = 8)) +
  theme(legend.position = "bottom") + 
  
  coord_map() + 
  facet_wrap(~Year, ncol = 3)
p

ggsave("Examples/SmallMultipleChoropleths.png", plot = last_plot(), width = 6, units = "in")
