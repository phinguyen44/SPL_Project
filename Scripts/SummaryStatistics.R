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
                     paste0("Age ", rep(c("50-54", "55-59", "60-64"), 2), c(rep(" ", 3), rep(" Obs"), 3)),
                     paste0(c("Chronic diseases", "ADLs", "Max. grip strength"), (" (mean)")),
                     paste0(c("Overweight", "Obese", " Bad mental health", "Good self-perceived health"),
                            " (in %)"))
# Comment: Can we improve creating summary statistics by looping over certain variables?
# Comment: Adla percentages are way too low compared to article!!!
# Comment: labour participation rates are way to low!!! I think this has to do with the
# assumption that 0 hours are interpreted as not working


################################################################################
# VISUALIZE SUMMARY STATISTICS IN TABLES

# TODO: create function for table output


DF <- data.frame(Ticker=c("", "", "", "IBM", "AAPL", "MSFT"),
                 Name=c("Dow Jones", "S&P 500", "Technology", 
                        "IBM", "Apple", "Microsoft"),
                 Value=accounting(c(15988.08, 1880.33, NA, 
                                    130.00, 97.05, 50.99)),
                 Change=percent(c(-0.0239, -0.0216, 0.021, 
                                  -0.0219, -0.0248, -0.0399)))

formattable(DF, list(
  Name=formatter(
    "row ",
    style = x ~ ifelse(x == "Technology", 
                       style(font.weight = "bold"), NA)),
  Value = color_tile("white", "green"),
  Change = above_mean_bold 
))


formattable(DF1, list(
  Observations = sign_formatter
            ))

# my formatting function
above_mean_bold = formatter("span", 
                            style = x ~ style("font-weight" = ifelse(x > mean(x), "bold", NA)))
DF1 = data.frame(sum.stats[3:5])

formattable(DF1, list(
  Observations = above_mean_bold
))

# not working

# formatting with inbuilt function
formattable(DF1, list(
  Observations = sign_formatter
))

# also not working: 
#Fehler in create_obj(x, "formattable", list(formatter = formatter, format = list(...),  : 
                                              #Objekt 'sign_formatter' nicht gefunden



