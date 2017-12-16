################################################################################
# SummaryStatistics.R
#
################################################################################
# Description:
# Summary statistics on health and labour force participation across countries
# 
################################################################################

# TODO: 
# Change functions to define level of aggregation (namely, get total)
# BOLD THE TOTALS

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

################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("dplyr", "magrittr", "purrr",
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
group.share = function(df, y, share.option = FALSE) {
    # give out group mean for variable by country as default
    if (share.option) {
        final = percent(tapply(df[[y]], df$country, function(x) {
            val = sum(x)/length(x)
            if (val > 1) {
                warning("Group percentage share exceeds defined range")
            }
            return(val)
        }))
    } else {
        final = tapply(df[[y]], df$country, function(x) {
            val = sum(x)/length(x)
            return(val)
        })
    }
    return(final)
}

# Function for calculating labor participation rate per country / gender / age
labor = function(df, .country, .gender, .age) {
    dataset = df %>% 
        filter(country == .country & gender == .gender & get(.age) == TRUE)
    denom = nrow(dataset)
    numer = with(dataset, sum(labor_ft | labor_pt))
    perc  = percent(round(numer/denom, 4))
    
    return(perc)
}

# use expand.grid to get all combinations of country / gender / age
agegroups = names(df.out[, 4:6])
args      = with(df.out, 
                 expand.grid(levels(country), levels(gender), agegroups, 
                             stringsAsFactors = FALSE))
args.list = list(c = as.vector(t(args[1])), 
                 g = as.vector(t(args[2])), 
                 a = as.vector(t(args[3])))

# Map over multiple arguments
output   = pmap(args.list, function(c, g, a) labor(df.out, c, g, a))
output.v = do.call("c", output)

labor.part.share    = cbind(args, output.v)
labor.part.share.df = spread(labor.part.share, Var3, output.v) %>% 
    arrange(desc(Var2)) %>% 
    set_colnames(c("Country", "Gender", agegroups))

# Labor supply choice tables
labor.supply.f = df.out %>% filter(gender == "FEMALE")
labor.supply.m = df.out %>% filter(gender == "MALE")

vars = names(df.out)[grep("labor", names(df.out))]
new = lapply(vars, function(x) group.share(df.out, x, 1))
labor.supply.m = lapply(vars, function(x) group.share(labor.supply.m[[x]],1))

# Creating summary statistics dataframe with percentage entries/mean
sum.stats = labor.m[1] %>% 
    mutate (
        observation   = summary(df.out$country),
        age50_54_p    = group.share(df.out$age50_54, 1),
        age55_59_p    = group.share(df.out$age55_59, 1),
        age60_64_p    = group.share(df.out$age60_64, 1),
        age50_54_n    = as.numeric(age50_54_p * observation), 
        age55_59_n    = as.numeric(age55_59_p * observation),
        age60_64_n    = as.numeric(age60_64_p * observation),
        h_chronic_p   = group.share(df.out$h_chronic),
        h_adla_p      = group.share(df.out$h_adla, 1),
        h_maxgrip_p   = group.share(df.out$h_maxgrip),
        h_overweigh_p = group.share(df.out$h_overweight, 1), 
        h_obese_p     = group.share(df.out$h_obese, 1),  
        h_badment_p   = group.share(df.out$h_badmental, 1),  
        h_goodsp_p    = group.share(df.out$h_goodsp, 1)
    ) %>% 
    set_rownames(levels(df.out$country)) %>% 
    select(-Country)

names(sum.stats) = c("Observations", 
                     paste0("Age ", rep(c("50-54", "55-59", "60-64"), 2), 
                            c(rep(" ", 3), rep(" Obs", 3))),
                     "Num chronic diseases (mean)",
                     "ADLs (in %)",
                     "Max grip strength (mean)", 
                     paste0(c("Overweight", "Obese", " Bad mental health", 
                              "Good self-perceived health"), " (in %)"))

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

# prepare all DF's
DF1 = sum.stats[, 1:4]
DF2 = sum.stats[, 8:10]
DF3 = sum.stats[, 11:14]
DF4 = labor.part.share.df %>% filter(Gender == "MALE") %>% select(-Gender)
DF5 = labor.part.share.df %>% filter(Gender == "FEMALE") %>% select(-Gender)

# TODO: Tables 4 - 6
# TODO: loop through tables, use map to insert totals, print all
sum.stats.out(sum.stats)
sum.stats.out(DF1)

################################################################################
# Export

# Source: https://stackoverflow.com/questions/38833219/command-for-exporting-saving-table-made-with-formattable-package-in-r

# Solution Style: as_htmlwidget and then print screen
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
