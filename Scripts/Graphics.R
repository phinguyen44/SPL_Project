################################################################################
# Graphics.R
#
################################################################################
# Description:
# Create useful graphics (Quantlet 6)
# 
# Graphics can be accessed numerous ways. 
# TODO: describe which ways?
# Functions are designed to work with df.out created from `ReadAndClean.R`
# 
# Quantlet enhances report by including numeric variables for which information
# is lost in conversion into dummary variables
# 
################################################################################

################################################################################
# SET WORKING DIRECTORY

# Note: Only this part must be changed for the rest of the script to run.
rm(list = ls())

# Adjust your working directory to where your local repository is located
wd = file.path("~/Documents/Projects/SPL_Project")
setwd(wd)

################################################################################
# SOURCE DATA

source("Scripts/ReadAndClean.R")
datasets = read.and.clean(dataset = "easySHARE_rel6_0_0.rda")

#Only keep relevant data sets
df.out = datasets$df.out
rm(datasets)

################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("dplyr", "magrittr", "ggplot2", "scales")
allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
    missingIDX = which(allPackages == FALSE)
    needed     = neededPackages[missingIDX]
    lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)

################################################################################
# BUILD GRAPHICS FUNCTION

# TODO: Each is a separate function

###### LABOR PARTICIPATION RATES ACROSS SEGMENTS
# Chloropeth of labor participation rates (men & women) (across age groups) - 6 total graphs. ALLOW FOR SELECTION OF T/F variable

###### VIEW DISTRIBUTION OF NUMERIC VARIABLES
# SELECT: Numeric variable: shows distribution for each country, then overlays average of entire data set (can potentially select one country, otherwise it defaults to all -> maybe pass a list of countries?)
# -> allow slicing by dummy variables

# by default, gender is "all". can either be "all", "MALE" or "FEMALE"
# by default, countries is "all". can also pass a character vector of countries
# by default, filters is "none". 
# - either pass a named logical vector containing predicates from data set
# - or a character vector like c("edu_low = FALSE", "age > 50"). allows for filters outside of just predicates

# TODO: Add option for no gen/country split?

health.distribution = function(var, 
                               gen       = "all", 
                               countries = "all", 
                               filters   = "none") {
    
    # first convert age to numeric, not factor variable
    df.out$age = as.numeric(levels(df.out$age)[df.out$age])
    
    # STOPPING CONDITIONS
    if (!is.numeric(df.out[[var]])) stop("'var' must be numeric")
    if (length(gen) > 1 | !all(gen %in% c("all", "MALE", "FEMALE"))) {
        stop("'gender' must be one of 'all', 'MALE', or 'FEMALE'")
    }
    if (length(countries) > 1 & !all(countries %in% levels(df.out$country))) {
        stop("'countries' must be 'all' or a character vector containing countries in the `df.out` data frame")
    }
    if (length(countries) == 1) {
        if (!(countries %in% c('all', levels(df.out$country)))) {
            stop("'countries' must be 'all' or a character vector containing countries in the `df.out` data frame")
        }
    }
    # TODO: Add filters stopping condition
    
    # TODO: Figure out how to do filters
    
    # Set faceting variables
    if (gen == "all") gen = c("MALE", "FEMALE")
    if (length(countries) == 1) {
        if (countries == "all") countries = levels(df.out$country)
    }
    
    # TODO: Overlay lines for average as well
    total.dist = table(df.out[[var]])/nrow(df.out)
    # TODO: Figure out how to do this for a histogram
    
    # if less than 10 values, use geom_bar. otherwise, use geom_hist.
    varlength = length(unique(df.out[[var]]))
    
    plot_bar = ggplot(data = df.out, aes(x = get(var))) + 
        geom_bar(aes(fill = country)) + 
        facet_grid(gender ~ country) +
        
        theme_minimal() +
        theme(legend.position = "none") + 
        theme(panel.grid.minor = element_blank()) + 
        theme(panel.grid.major.x = element_blank()) +
        
        labs(title = "Distribution of ...") +
        labs(subtitle = "Text2") +
        
        theme(plot.title = element_text(size=16)) +
        theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
    
    plot_hist = ggplot(data = df.out, aes(x = get(var))) + 
        geom_histogram(bins = 10, aes(fill = country)) + 
        facet_grid(gender ~ country) + 
        
        theme_minimal() +
        theme(legend.position = "none") + 
        theme(panel.grid.minor = element_blank()) + 
        theme(panel.grid.major.x = element_blank()) +
        
        labs(title = "Distribution of ...") +
        labs(subtitle = "Text2") +
        
        theme(plot.title = element_text(size=16)) +
        theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
    
    if (varlength <= 10) {
        plotter = plot_bar
    } else {
        plotter = plot_hist
    }
    
    plotter
    
    return(plotter)
    
    
    
}

###### VIEW LABOR CHOICE (AS DIVERGING STACKED BAR CHART)
# Decide best graph to show. Could be stacked bar, could be bars separate, could be left-side as non working and right-side as working

# I like the idea of "tranching" people into categories based on health outcomes. Maybe we'll see something like how really healthy people have amazing labor rates, but unhealthy people have super garbage rates. And that maybe incremental gains from the unhealthy population will lead to higher participation rates, etc. (How to influence policy!) - inequality of health. For that we'll need to run regression on separate buckets of health outcomes