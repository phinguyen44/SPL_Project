################################################################################
# Probit.Regression
#
################################################################################
# Description:
# 
# 
################################################################################

################################################################################

# Comment: How should we start this quantlet? Load the pre-processd data set?
# Specify with data must be loaded? 


rm(list= ls()[!(ls() %in% c("df.reg", "df.splits"))])


################################################################################
# LOAD NECESSARY PACKAGES & DATA

# List all packages needed for session
neededPackages = c()
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

# Try out regression for German men
mydf = data.frame(df.splits[15])
names(mydf)

RegModel = glm(DEU.MALE.labor_participationTRUE ~ .- DEU.MALE.age50, family = binomial(link = "probit"), 
                data = mydf)
 
summary(RegModel)


sum(mydf$DEU.MALE.labor_participationTRUE)/length(mydf$DEU.MALE.labor_participationTRUE)