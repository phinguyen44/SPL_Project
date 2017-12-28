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

health.distribution = function(xvar, 
                               gen              = "all", 
                               countries        = "all", 
                               filters          = "none",
                               remove.outliers  = TRUE) {
    
    # first convert age to numeric, not factor variable
    df.out$age = as.numeric(levels(df.out$age)[df.out$age])
    
    # STOPPING CONDITIONS
    if (!is.numeric(df.out[[xvar]])) stop("'xvar' must be numeric")
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
    
    # Remove outliers (outside 1.5 IQR of 25% and 75% percentiles)
    rm.outliers = function(x, na.rm = TRUE) {
        qnt = quantile(df.out[[x]], probs = c(0.25, 0.75), na.rm = na.rm)
        out = 1.5*IQR(df.out[[x]], na.rm = na.rm)
        
        new.out = df.out
        new.out[(df.out[[x]] < qnt[1] - out), ] = NA
        new.out[(df.out[[x]] > qnt[2] + out), ] = NA

        new.out = new.out[complete.cases(new.out), ]
        return(new.out)
    }
    
    if (remove.outliers) {
        df.out = rm.outliers(x = xvar)
    }
    
    # TODO: Overlay lines for average as well
    total.dist = table(df.out[[xvar]])/nrow(df.out)
    
    # TODO: show as percentage
    arg = df.out %>% 
        group_by(country, gender) %>% 
        mutate(perc = )

    plot.bar = ggplot(data = df.out, aes(x = get(xvar))) + 
        geom_bar(aes(fill = country, color = country), alpha = 0.4) + 
        facet_grid(gender ~ country) +
        
        theme_minimal() +
        theme(legend.position = "none") + 
        theme(panel.grid.minor = element_blank()) + 
        theme(panel.grid.major.x = element_blank()) +
        
        labs(title = "Distribution of ...") +
        labs(subtitle = "Text2") +
        
        theme(plot.title = element_text(size=16)) +
        theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
    
    # 
    
    plot.bar
    
    return(plot.bar)
    
}

###### VIEW LABOR CHOICE (AS DIVERGING STACKED BAR CHART)
# Decide best graph to show. Could be stacked bar, could be separate bars, could be diveging (like a Likert scale)

# I like the idea of "tranching" people into categories based on health outcomes. Maybe we'll see something like how really healthy people have amazing labor rates, but unhealthy people have super garbage rates. And that maybe incremental gains from the unhealthy population will lead to higher participation rates, etc. (How to influence policy!) - inequality of health. For that we'll need to run regression on separate buckets of health outcomes