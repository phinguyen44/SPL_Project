################################################################################
# SummaryStatistics.R
#
################################################################################
# Description:
# Summary statistics on health and labour force participation across countries
# 
################################################################################

################################################################################

#SET WORKING DIRECTORY
  
#Note: Only this part must be changed for the rest of the script to run.
  
rm(list = ls())

#Adjust your working directory to where your local repository is located
  
wd = file.path("~/Documents/Projects/SPL_Project")
setwd(wd)
  
  ################################################################################
  
#SOURCE DATA*

#source from quantlet 1
    
source("Scripts/ReadAndClean.R") # not working

  
#Only keep relevant data sets
    
rm(list= ls()[!(ls() %in% c("df.out"))])


# Ideas for functions:
# Creates table that shows percentages within each stratum / substratum
# function selecting metric and gives labour participation rate as output
# tables could also use conditional formatting -> colour entries 


################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("dplyr", "formattable", "webshot", "htmltools" )
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
# Function gives out mean as default and percentage if any additional option entry is provided

group.share = function(y, share.option = NULL) {
  
# give out group mean for variable by country as default
  if (is.null(share.option)){
         tapply(X= y, 
           INDEX = df.out$country, 
           FUN   = function(x) {
             val = sum(x)/length(x)
     return(val)
           })
    
# for (any) further option entry return percentage
  } else {perc = percent(
          tapply(X= y, 
            INDEX = df.out$country, 
            FUN   = function(x) {
              val = sum(x)/length(x)
     return(val)   
  }))
  return(perc)
}}

# Function for calculating labor particpation rate per country
labor.part.share = by(df.out, list(df.out$country), function(z){
  
  # Create Index Vector seperated by gender
  IDX_w_l = ifelse({z$gender=="FEMALE"} & {z$labor_ft | z$labor_pt}, TRUE, FALSE)
  IDX_m_l = ifelse({z$gender=="MALE"}   & {z$labor_ft | z$labor_pt}, TRUE, FALSE)
  
  # Count 
  working_w = sum(ifelse(IDX_w_l, 1, 0))
  working_m = sum(ifelse(IDX_m_l, 1, 0))
  
  # Get Length in order to calculate percentages
  len_w = length(IDX_w_l)
  len_m = length(IDX_m_l)
  
  # Calculate percentages
  perc_w = working_w / len_w
  perc_m = working_m / len_m
  
  # Create Names
  output.list = list(perc_w, perc_m)
  names(output.list) = c(paste0(c("Female", "Male"), " Labor Participation Share"))
  
  return(output.list)
  
})

labor.part.share.df = do.call(rbind.data.frame, labor.part.share)
names(labor.part.share.df) = c(paste0(c("Female", "Male"), " Labor Participation Share"))

# Creating summary statistics dataframe with percentage entries/mean
sum.stats = data.frame(
          cbind(labor.part.share.df, matrix(nrow = 11, ncol = 0)) %>% 
                mutate(observation  = summary(df.out$country),
                       age50_54_p   =   group.share(df.out$age50_54, 1),
                       age55_59_p   =   group.share(df.out$age55_59, 1),
                       age60_64_p   =   group.share(df.out$age60_64, 1),
                       age50_54_n   =   as.numeric(age50_54_p * observation), 
                       age55_59_n   =   as.numeric(age55_59_p * observation),
                       age60_64_n   =   as.numeric(age60_64_p * observation),
                       h_chronic_p  =   group.share(df.out$h_chronic),
                       h_maxgrip_p  =   group.share(df.out$h_maxgrip),
                       h_adla_p     =   group.share(df.out$h_adla),
                       h_overweigh_p=   group.share(df.out$h_overweight, 1),  
                       h_obese_p    =   group.share(df.out$h_obese, 1),  
                       h_badment_p  =   group.share(df.out$h_badmental, 1),  
                       h_goodsp_p   =   group.share(df.out$h_goodsp, 1)))

rownames(sum.stats) = levels(df.out$country)

names(sum.stats) = c(paste0(c("Female", "Male"), " Labor Participation Share"),
                     "Oberservations",
                     paste0("Age ", rep(c("50-54", "55-59", "60-64"), 2), c(rep(" ", 3), rep(" Obs", 3))),
                     paste0(c("Chronic diseases", "Max. grip strength", "ADLs"), (" (mean)")),
                     paste0(c("Overweight", "Obese", " Bad mental health", "Good self-perceived health"),
                            " (in %)"))

# Comment: Can we improve creating summary statistics by looping over certain variables?
# Comment: Adla percentages are way too low compared to article!!!

# Comment: labour participation rates are way to low!!! I think this has to do with the


################################################################################
# VISUALIZE SUMMARY STATISTICS IN TABLES


# Formatting function for showing entries above mean in bold
above_mean_bold = formatter("span", 
                           style = i ~ style("font-weight" = ifelse(i > mean(i), "bold", NA)))


# Creating a summary statistics table with conditional formatting
sum.stats.out = function(DF){
  
  # create an ouput table based on row-specific criteria
  formattable(DF, lapply(DF, function(x) {
  
  # show above mean entries for non-percentage variables in bold
    if (max(x) > 1  & max (x) < 100){
        formatter("span", style = i ~ style("font-weight" = ifelse(i > mean(i), "bold", NA)))
      
  # apply conditional formatting to percentage variables by coloring
    } else if (max (x) <= 1){
        color_tile('white', 'lightblue')
      
  # Leave number of observations unformatted
      } else {
          formatter("span", style = NA)
        
  # Specify table options: aligned numbers, 2 decimal digits with floating numbers
}}), options(digits=3, format="d"), align = "c"
)} # Attention: format "d" is not being correctly recognized, decimal digits not displayed for max grip

DF1 = sum.stats[3:6]

sum.stats.out(sum.stats)


sum.stats.out(DF1)

################################################

# Source: https://stackoverflow.com/questions/38833219/command-for-exporting-saving-table-made-with-formattable-package-in-r

# Solution Style: as_htmlwidget and then print screen

# Required for export_formattable

library("htmltools") # for html_print
library("webshot")
install_phantomjs()

# TODO
# Change layout of output table: adjust when variable name is large
# Show percentage entries
# Show name of countries
# Maybe: Improve creationg of summary statistics (e.g. lapply)

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(sum.stats.out(DF1), file = "test.png")

