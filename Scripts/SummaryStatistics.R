################################################################################
# SummaryStatistics.R
#
################################################################################
# Description:
# Summary statistics on health and labour force participation across countries
# 
################################################################################

# TODO: 
# Clean up code... it's hideous
# Lapply the group.share fxn for the sum.stats table?
# Formatting of tables: Bold, fix column widths, add titles

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
# CREATE DATA FRAME FOR SUMMARY STATISTICS

# Function for calculation group shares (percentage or mean) per country
# Function gives out mean as default and percentage if any additional option
# entry is provided
group.share = function(df, y, share.option = FALSE, agg.country = TRUE) {
    # give out group mean for variable by country as default
    if (agg.country) {
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
    } else { # return total if country-level aggregation not specified
        if (share.option) {
            final = percent(sum(df[[y]])/length(df[[y]]))
            if (final > 1) {
                warning("Group percentage share exceeds defined range")
            }
        } else {
            final = sum(df[[y]])/length(df[[y]])
            }
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
df.female = df.out %>% filter(gender == "FEMALE")
df.male   = df.out %>% filter(gender == "MALE")
vars      = names(df.out)[grep("labor", names(df.out))][3:1] # invert

labor.supply.f    = lapply(vars, function(x) group.share(df.female, x, 1))
labor.supply.f.df = do.call(cbind.data.frame, labor.supply.f) %>% 
    set_colnames(c("Nonparticipation", "Half time", "Full time"))

labor.supply.m    = lapply(vars, function(x) group.share(df.male, x, 1))
labor.supply.m.df = do.call(cbind.data.frame, labor.supply.m) %>% 
    set_colnames(c("Nonparticipation", "Half time", "Full time"))

# Creating summary statistics dataframe with percentage entries/mean
sum.stats = labor.part.share.df[,c(1,2)] %>% 
    filter(Gender == "MALE") %>% 
    mutate (
        observation   = summary(df.out$country),
        age50_54_p    = group.share(df.out, "age50_54", 1),
        age55_59_p    = group.share(df.out, "age55_59", 1),
        age60_64_p    = group.share(df.out, "age60_64", 1),
        h_chronic_p   = group.share(df.out, "h_chronic"),
        h_adla_p      = group.share(df.out, "h_adla", 1),
        h_maxgrip_p   = group.share(df.out, "h_maxgrip"),
        h_overweigh_p = group.share(df.out, "h_overweight", 1), 
        h_obese_p     = group.share(df.out, "h_obese", 1),  
        h_badment_p   = group.share(df.out, "h_badmental", 1),  
        h_goodsp_p    = group.share(df.out, "h_goodsp", 1)
    ) %>% 
    set_rownames(levels(df.out$country)) %>% 
    select(-Gender, -Country)

names(sum.stats) = c("Observations", 
                     paste0("Age ", c("50-54", "55-59", "60-64")),
                     "Num chronic diseases (mean)",
                     "ADLs (in %)",
                     "Max grip strength (mean)", 
                     paste0(c("Overweight", "Obese", " Bad mental health", 
                              "Good self-perceived health"), " (in %)"))

################################################################################
# GET TOTALS

## Total labor participation rate
total.labor = function(df, .gender, .age) {
    dataset = df %>% 
        filter(gender == .gender & get(.age) == TRUE)
    denom = nrow(dataset)
    numer = with(dataset, sum(labor_ft | labor_pt))
    perc  = percent(round(numer/denom, 4))
    
    return(perc)
}

# use expand.grid to get all combinations of country / gender / age
args.tot      = with(df.out, 
                     expand.grid(levels(gender), agegroups, 
                                 stringsAsFactors = FALSE))
args.list.tot = list(g = as.vector(t(args.tot[1])), 
                     a = as.vector(t(args.tot[2])))

# Map over multiple arguments
output.tot   = pmap(args.list.tot, function(g, a) total.labor(df.out, g, a))
output.v.tot = do.call("c", output.tot)

total.labor.part    = cbind(args, output.v)
total.labor.part.df = spread(total.labor.part, Var2, output.v) %>% 
    arrange(desc(Var1)) %>% 
    set_colnames(c( "Gender", agegroups))

## TOTAL Labor supply choice tables
tot.supply.f    = lapply(vars, function(x) group.share(df.female, x, 1, 0))
tot.supply.f.df = do.call(cbind.data.frame, tot.supply.f) %>% 
    set_colnames(c("Nonparticipation", "Half time", "Full time"))

tot.supply.m    = lapply(vars, function(x) group.share(df.male, x, 1, 0))
tot.supply.m.df = do.call(cbind.data.frame, tot.supply.f) %>% 
    set_colnames(c("Nonparticipation", "Half time", "Full time"))

## TOTAL sum stats
perc.vars = c("age50_54", "age55_59", "age60_64",
              "h_adla", "h_overweight", "h_obese", "h_badmental", "h_goodsp")
num.vars  = c("h_chronic", "h_maxgrip")

obs      = nrow(df.out)
percs    = lapply(perc.vars, function(x) group.share(df.out, x, 1, 0))
nums     = lapply(num.vars, function(x) group.share(df.out, x, 0, 0))
all.list = list(obs, percs, nums)
sum.stats.tot = do.call(cbind.data.frame, all.list) %>% 
    select(1, 2, 3, 4, 10, 5, 11, 6, 7, 8, 9, 10) %>% # reorder
    set_colnames(names(sum.stats))

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
DF2 = sum.stats[, 5:7]
DF3 = sum.stats[, 8:11]
DF4 = labor.part.share.df %>% 
    filter(Gender == "MALE") %>% 
    set_rownames(levels(df.out$country)) %>% 
    select(-Gender, -Country) %>% 
    set_colnames(paste0("Age ", rep(c("50-54", "55-59", "60-64"))))
DF5 = labor.part.share.df %>% 
    filter(Gender == "FEMALE") %>% 
    set_rownames(levels(df.out$country)) %>% 
    select(-Gender, -Country) %>% 
    set_colnames(paste0("Age ", rep(c("50-54", "55-59", "60-64"))))
DF6 = labor.supply.m.df
DF7 = labor.supply.f.df

listDF = list(DF1=DF1, DF2=DF2, DF3=DF3, DF4=DF4, DF5=DF5, DF6=DF6, DF7=DF7)

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

map2(listDF, names(listDF), function(a, b) {
    export_formattable(sum.stats.out(a), file = paste0("Output/", b, ".png"))
})
