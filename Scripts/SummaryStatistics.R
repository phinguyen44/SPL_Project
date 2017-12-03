################################################################################
# SummaryStatistics.R
#
################################################################################
# Description:
# Summary statistics on health and labour force participation across countries
# 
################################################################################

################################################################################

# Comment: How should we start this quantlet? Load the pre-processd data set?
# Specify with data must be loaded? 


rm(list= ls()[!(ls() %in% c("df.out"))])


# Ideas for functions:
# Creates table that shows percentages within each stratum / substratum
# function selecting metric and gives labour participation rate as output
# tables could also use conditional formatting -> colour entries 


################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c("dplyr", "formattable")
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



# Function for calculation group percentages per country
group.percentage     = function(y){tapply(X = y, INDEX = df.out$country, 
                      FUN = function(x){
                        val = percent(sum(x)/length(x))
                        return(val)
                      })} 


# TODO: why is this not return a percentage entry?

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
                       age50_54_p   =   group.percentage(df.out$age50_54),
                       age55_59_p   =   group.percentage(df.out$age55_59),
                       age60_64_p   =   group.percentage(df.out$age60_64),
                       age50_54_n   =   age50_54_p * observation, 
                       age55_59_n   =   age55_59_p * observation,
                       age60_64_n   =   age60_64_p * observation,
                       h_chronic_p  =   group.percentage(df.out$h_chronic),
                       h_maxgrip_p  =   group.percentage(df.out$h_maxgrip),
                       h_adla_p     =   group.percentage(df.out$h_adla),
                       h_overweigh_p=   group.percentage(df.out$h_overweight),  
                       h_obese_p    =   group.percentage(df.out$h_obese),  
                       h_badment_p  =   group.percentage(df.out$h_badmental),  
                       h_goodsp_p   =   group.percentage(df.out$h_goodsp)))

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
# assumption that 0 hours are interpreted as not working


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
          
}}))}

DF1 = data.frame(sum.stats[3:6])

sum.stats.out(sum.stats)
sum.stats.out(DF1)

# TODO
# Change layout of output table: adjust when variable name is large
# Show percentage entries
# Show name of countries


