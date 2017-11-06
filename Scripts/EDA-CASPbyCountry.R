#################################################################################################
# EDA-CASPbyCountry.R
#
#################################################################################################
# Description:
# Grid map: displays average CASP score of countries in Wave 5 of SHARE.
# Grid map #2: plot histograms
# Maps of distributions
# Na's are removed
#
#################################################################################################

#################################################################################################
# LOAD DATA

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/SPL_Project")
setwd(wd)

# Source necessary packages and data
source("Scripts/EDA.R")
rm(list = ls()[which(!(ls() %in% "df.loc"))])
needs(ggplot2, countrycode, viridis, geofacet, ggridges)

##################################################################################################
# Data Preparation

casp_country = df.loc %>% 
  group_by(loc_country) %>% 
  summarize(avg_casp = round(mean(casp, na.rm = TRUE),2))

coordinates = readxl::read_excel("Examples/Coordinates.xlsx", sheet = 1)
codes       = countrycode_data

df = left_join(coordinates, codes, by = c("Country" = "country.name.en")) %>%
  select(Country, iso3c, X, Y) %>%
  left_join(casp_country, by = c("iso3c" = "loc_country")) %>%
  rename(loc_country = iso3c)

# remove Israel
df.loc = df.loc %>% filter(loc_country != "ISR")

# remove NA's and reorder
df.noNA     = df %>% filter(!is.na(avg_casp))
df.loc.noNA = df.loc %>% 
  filter(!is.na(casp)) %>% 
  group_by(loc_country) %>%
  mutate(m = mean(casp)) %>%
  arrange(m) %>%
  ungroup() %>%
  mutate(loc_country=factor(loc_country, unique(loc_country)))

##################################################################################################
# Draw graph

p = ggplot(data = df, aes(x = X, y = Y)) +
  geom_tile(aes(fill = avg_casp), color = "white", size = 0.6) +
  geom_text(aes(label = loc_country), color = "white") +
  geom_text(aes(label = avg_casp), vjust = 2, color = "white", size = 3.5, na.rm = TRUE) +

  scale_fill_continuous(low = "#fdd49e", high = "#b30000") +

  labs(title    = "Average CASP Score by Country",
       subtitle = "Data: Wave 5, NA's removed",
       fill     = "CASP Score") +

  coord_fixed(ratio = 1) +

  theme_minimal() +
  theme(axis.line        = element_blank(),
        axis.text        = element_blank(),
        axis.title       = element_blank(),
        panel.background = element_blank(),
        panel.grid       = element_blank(),
        legend.position  = "bottom") +
  theme(plot.title    = element_text(size=16),
        plot.subtitle = element_text(size=10, color = "#7F7F7F"))
p

ggsave("Output/GridMap-CASPbyCountry.png", plot = last_plot(), width = 6, height = 6, units = "in")

##################################################################################################
# Draw graph2

p2 = ggplot(data = df.loc.noNA, aes(x = casp)) +
  geom_histogram(aes(y=..density..), bins = 10) +
  geom_vline(data = df.noNA, aes(xintercept = avg_casp), color = "red") +
  geom_label(data = df.noNA, aes(x = avg_casp, y = 0, label = avg_casp), nudge_y = 0.13, size = 3, color = "red") +

  scale_y_continuous(labels = scales::percent, limits = c(0, 0.15)) +

  theme_bw() +
  theme(axis.title       = element_blank(),
        panel.background = element_blank(),
        panel.grid       = element_blank()) +

  facet_wrap(~loc_country) +

  labs(title = "Distribution of CASP Scores by Country")
p2

ggsave("Output/Histogram-CASPbyCountry.png", plot = last_plot(), width = 6, height = 6, units = "in")

##################################################################################################
# Draw graph3

p3 = ggplot(data = df.loc.noNA, aes(x = casp, y = loc_country)) +
  geom_density_ridges(fill = "lightblue", na.rm = TRUE, scale = 2, rel_min_height = 0.01) +
  # scale_x_continuous(limits = c(20, NA)) +
  scale_y_discrete(expand = c(0.01, 0)) +

  theme_ridges(grid = FALSE) +
  theme(axis.title = element_blank()) +

  labs(title = "Distribution of CASP Scores by Country")
p3
# TODO: order by mean? color by mean value?

ggsave("Output/Distribution-CASPbyCountry.png", plot = last_plot(), width = 6, height = 8, units = "in")

## TODO: geofacet