################################################################################
# SummaryStatistics.R
#
################################################################################
# Description:
# Summary statistics on health and labour force participation across countries
# 
################################################################################

# TODO: 
# Change layout of output table: adjust when variable name is large
# Show percentage entries
# Maybe: Improve creation of summary statistics (e.g. lapply)
# Question: Why do we define names for labor.part.share twice?
# Caution: group.percentage function needs additional option entry as numeric

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

# Ideas for functions:
# Creates table that shows percentages within each stratum / substratum
# function selecting metric and gives labour participation rate as output
# tables could also use conditional formatting -> colour entries 

################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("dplyr", "magrittr", 
                   "formattable", "webshot", "htmltools", "webshot")
allPackages    = c(neededPackages %in% installed.packages()[,"Package"]) 

# Install packages (if not already installed) 
if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, library, character.only = TRUE)

################################################################################
# CREATE DATA FRAME FOR SUMMARY STATISTICS

# Function for calculation group shares (percentage or mean) per country
# Function gives out mean as default and percentage if any additional option
# entry is provided

group.share = function(y, share.option = FALSE) {
    # give out group mean for variable by country as default
    if (share.option) {
        perc = percent(tapply(y, df.out$country, function(x) {
            val = sum(x)/length(x)
            if (val > 1) {
                warning("Group percentage share exceeds defined range")
            }
            return(val)
        }))
    } else {
        tapply(y, df.out$country, function(x) {
            sum(x)/length(x)
        })
    }
}

# Function for calculating labor participation rate per country
labor.part.fxn = function(z) {
    
    # Create Index Vector separated by gender
    IDX_w_l = with(z, ifelse(
        gender == "FEMALE" & (labor_ft | labor_pt), TRUE, FALSE))
    IDX_m_l = with(z, ifelse(
        gender == "MALE" & (labor_ft | labor_pt), TRUE, FALSE))
    
    # Calculate percentages
    perc_w = round(sum(IDX_w_l) / length(IDX_w_l), 4)
    perc_m = round(sum(IDX_m_l) / length(IDX_m_l), 4)
    
    # Create Names
    output.list = list(perc_w, perc_m)
    names(output.list) = c(
        paste0(c("Female", "Male"), " Labor Participation Share")
    )
    return(output.list)
}

labor.part.share    = by(df.out, list(df.out$country), labor.part.fxn)
labor.part.share.df = do.call(rbind.data.frame, labor.part.share) %>% 
    set_colnames(c(paste0(c("Female", "Male"), " Labor Participation Share")))
# TODO: format as percentage

# Creating summary statistics dataframe with percentage entries/mean
sum.stats = labor.part.share.df %>% 
    mutate(
        observation   = summary(df.out$country),
        age50_54_p    = group.share(df.out$age50_54, 1),
        age55_59_p    = group.share(df.out$age55_59, 1),
        age60_64_p    = group.share(df.out$age60_64, 1),
        age50_54_n    = as.numeric(age50_54_p * observation), 
        age55_59_n    = as.numeric(age55_59_p * observation),
        age60_64_n    = as.numeric(age60_64_p * observation),
        h_chronic_p   = group.share(df.out$h_chronic),
        h_maxgrip_p   = group.share(df.out$h_maxgrip),
        h_adla_p      = group.share(df.out$h_adla, 1),
        h_overweigh_p = group.share(df.out$h_overweight, 1),  
        h_obese_p     = group.share(df.out$h_obese, 1),  
        h_badment_p   = group.share(df.out$h_badmental, 1),  
        h_goodsp_p    = group.share(df.out$h_goodsp, 1)
    ) %>% 
    set_rownames(levels(df.out$country))

names(sum.stats) = c(paste0(c("Female", "Male"), " Labor Participation Share"),
                     "Observations", 
                     paste0("Age ", rep(c("50-54", "55-59", "60-64"), 2), 
                            c(rep(" ", 3), rep(" Obs", 3))),
                     paste0(c("Chronic diseases", "Max. grip strength", "ADLs"),
                            (" (mean)")),
                     paste0(c("Overweight", "Obese", " Bad mental health", 
                              "Good self-perceived health"), " (in %)"))

# TODO: Can we improve creating summary statistics by looping over certain
# variables? Yea - just use df.out as base then rbind labor.part.share.df

################################################################################
# VISUALIZE SUMMARY STATISTICS IN TABLES

# set formats
format.settings = function(x) {
    if (max(x) > 1 & max(x) < 100) {
        # show above mean entries for non-percentage variables in bold
        formatter(
            "span",
            style = i ~ style("font-weight" = ifelse(i > mean(i), "bold", NA))
        )
    } else if (max(x) <= 1) {
        # apply conditional formatting to percentage variables by coloring
        color_tile("white", "lightblue")
    } else {
        # Leave number of observations unformatted
        formatter("span", style = NA)
    }
}
# Create a summary statistics table with conditional formatting
sum.stats.out = function(DF) {
    # create an ouput table based on row-specific criteria
    formattable(DF, lapply(DF, format.settings), 
                options(digits = 3, format = "d"),
                align = "c")
}

DF1 = sum.stats[3:6]
sum.stats.out(sum.stats)
sum.stats.out(DF1)

################################################################################
# Export

# Source: https://stackoverflow.com/questions/38833219/command-for-exporting-saving-table-made-with-formattable-package-in-r

# Solution Style: as_htmlwidget and then print screen
install_phantomjs()

export_formattable = function(f, file, width = "100%", height = NULL, 
                              background = "white", delay = 0.2) {
    w    = as.htmlwidget(f, width = width, height = height)
    path = html_print(w, background = background, viewer = NULL)
    url  = paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
    webshot(url,
            file = file,
            selector = ".formattable_widget",
            delay = delay)
}

export_formattable(sum.stats.out(DF1), file = "test.png")