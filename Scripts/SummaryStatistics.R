################################################################################
# SummaryStatistics.R
#
################################################################################
# Description:
# Summary statistics on health and labour force participation across countries
# 
################################################################################

# TODO: 
# Apply percent formatting at the very end
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

# Function for calculation group shares per country
group.share = function(df, y, agg.country = TRUE) {
    if (agg.country) {
        final = tapply(df[[y]], df$country, function(x) {
            val = sum(x)/length(x)
            return(val)
        })
    } else { # return total if country-level aggregation not specified
        final = sum(df[[y]])/length(df[[y]])
    }
    return(final)
}

# Function for calculating labor participation rate per country / gender / age
labor = function(df, .country, .gender, .age) {
    dataset = df %>% 
        filter(country == .country & gender == .gender & get(.age) == TRUE)
    denom = nrow(dataset)
    numer = with(dataset, sum(labor_ft | labor_pt))
    perc  = round(numer/denom, 4)
    
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
    set_colnames(c("Country", "Gender", 
                   paste0("Age ", rep(c("50-54", "55-59", "60-64")))))

# Get gender specific tables
labor.part.m = labor.part.share.df %>% 
    filter(Gender == "MALE") %>% 
    set_rownames(levels(df.out$country)) %>% 
    select(-Gender, -Country)
labor.part.f = labor.part.share.df %>% 
    filter(Gender == "FEMALE") %>% 
    set_rownames(levels(df.out$country)) %>% 
    select(-Gender, -Country)

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
vars.vec  = c("age50_54", "age55_59", "age60_64",
              "h_chronic", "h_adla", "h_maxgrip", "h_overweight", "h_obese",
              "h_badmental", "h_goodsp")
names.vec = c("Observations", 
               paste0("Age ", c("50-54", "55-59", "60-64")),
               "Num chronic diseases (mean)",
               "ADLs (in %)",
               "Max grip strength (mean)", 
               paste0(c("Overweight", "Obese", " Bad mental health", 
                        "Good self-perceived health"), " (in %)"))

obs.stats      = summary(df.out$country)
stats          = lapply(vars.vec, function(x) group.share(df.out, x, 1))
list.stats     = list(obs.stats, stats)
sum.stats      = do.call(cbind.data.frame, list.stats) %>%
    set_colnames(names.vec)

################################################################################
# GET TOTALS

## Total labor participation rate
total.labor = function(df, .gender, .age) {
    dataset = df %>% 
        filter(gender == .gender & get(.age) == TRUE)
    denom = nrow(dataset)
    numer = with(dataset, sum(labor_ft | labor_pt))
    perc  = round(numer/denom, 4)
    
    return(perc)
}

# use expand.grid to get all combinations of gender / age
args.tot      = with(df.out, 
                     expand.grid(levels(gender), agegroups, 
                                 stringsAsFactors = FALSE))
args.list.tot = list(g = as.vector(t(args.tot[1])), 
                     a = as.vector(t(args.tot[2])))

# Map over multiple arguments
output.tot   = pmap(args.list.tot, function(g, a) total.labor(df.out, g, a))
output.v.tot = do.call("c", output.tot)

total.labor.part    = cbind(args.tot, output.v.tot)
total.labor.part.df = spread(total.labor.part, Var2, output.v.tot) %>% 
    arrange(desc(Var1)) %>% 
    set_colnames(c( "Gender", 
                    paste0("Age ", rep(c("50-54", "55-59", "60-64")))))

# Get gender specific tables
labor.part.m.tot = total.labor.part.df %>% 
    filter(Gender == "MALE") %>% 
    set_rownames("TOTAL") %>% 
    select(-Gender)
labor.part.f.tot = total.labor.part.df %>% 
    filter(Gender == "FEMALE") %>% 
    set_rownames("TOTAL") %>% 
    select(-Gender)

## TOTAL Labor supply choice tables
tot.supply.f    = lapply(vars, function(x) group.share(df.female, x, 0))
tot.supply.f.df = do.call(cbind.data.frame, tot.supply.f) %>% 
    set_colnames(c("Nonparticipation", "Half time", "Full time")) %>% 
    set_rownames("TOTAL")

tot.supply.m    = lapply(vars, function(x) group.share(df.male, x, 0))
tot.supply.m.df = do.call(cbind.data.frame, tot.supply.f) %>% 
    set_colnames(c("Nonparticipation", "Half time", "Full time")) %>% 
    set_rownames("TOTAL")

## TOTAL sum stats
obs.tot       = nrow(df.out)
stats.tot     = lapply(vars.vec, function(x) group.share(df.out, x, 0))
tot.list      = list(obs.tot, stats.tot)
sum.stats.tot = do.call(cbind.data.frame, tot.list) %>% 
    set_colnames(names.vec) %>% 
    set_rownames("TOTAL")

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
DF1 = rbind(sum.stats[, 1:4], sum.stats.tot[, 1:4])
DF2 = rbind(sum.stats[, 5:7], sum.stats.tot[, 5:7])
DF3 = rbind(sum.stats[, 8:11], sum.stats.tot[, 8:11])
DF4 = rbind(labor.part.m, labor.part.m.tot)
DF5 = rbind(labor.part.f, labor.part.f.tot)
DF6 = rbind(labor.supply.m.df, tot.supply.m.df)
DF7 = rbind(labor.supply.f.df, tot.supply.m.df)

# format as percentage

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
