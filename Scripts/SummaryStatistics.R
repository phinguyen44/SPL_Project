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
group.percentage     = function(x){tapply(X = x, INDEX = df.out$country, 
                      FUN = function(x){
                        val = sum(x)/length(x)
                        return(val)
                      })} 

# Creates index vector for working woman and men (half or full time)
IDX_w_l = ifelse({df.out$gender=="FEMALE"} & {df.out$labor_ft | df.out$labor_pt}, TRUE, FALSE)
IDX_m_l = ifelse({df.out$gender=="MALE"}   & {df.out$labor_ft | df.out$labor_pt}, TRUE, FALSE)
k = list(levels(df.out$country))

# Function for calculating labor particpation rate per country
labor.part.percentage = function(x, i){tapply(X = x, INDEX = df.out$country, 
                                          FUN = function(x, i){ # does no recognize index i
                                              working = sum(ifelse(i, 1, 0))
                                              all     = length(x)
                                              val = working/all
                                            return(val)
                                          })}

# there is an error in the function: not returning labor rate but age percentage


# Creating summary statistics dataframe with percentage entries/mean
sum.stats = 
      data.frame(matrix(nrow = 11, ncol = 0)) %>% 
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
             h_goodsp_p   =   group.percentage(df.out$h_goodsp),
             age50_54_ml_p=   labor.part.percentage(df.out$age50_54, IDX_m_l),
             age55_59_ml_p=   labor.part.percentage(df.out$age55_59, IDX_m_l),
             age60_64_ml_p=   labor.part.percentage(df.out$age60_64, IDX_m_l),
             age50_54_wl_p=   labor.part.percentage(df.out$age50_54, IDX_w_l),
             age55_59_wl_p=   labor.part.percentage(df.out$age55_59, IDX_w_l),
             age60_64_wl_p=   labor.part.percentage(df.out$age60_64, IDX_w_l))

rownames(sum.stats) = levels(df.out$country)
# Comment: Can we improve creating summary statistics by looping over certain variables?
# Comment: Adla percentages are way too low compared to article!!!
# Comment: labour participation rates are way to low!!! I think this has to do with the
# assumption that 0 hours are interpreted as not working







################################################################################
# VISUALIZE SUMMARY STATISTICS IN TABLES

# TODO: create function for table output


k = function(y){formattable(x = y)}  

k(sum.stats[,1:4])

