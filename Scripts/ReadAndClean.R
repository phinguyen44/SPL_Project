################################################################################
# ReadAndClean.R
#
################################################################################
# Description:
# Function loads easySHARE data set and outputs relevant data frames for
# analysis and regression. Additionally, print error messages if data set is
# not the correct easySHARE data set and relevant statistics printed (e.g. num
# rows dropped, variables retained, data type)
#
################################################################################

read.and.clean <- function(dataset = "easySHARE_rel6_0_0.rda", wav = 1) {

    # LOAD NECESSARY PACKAGES & DATA
    # List all packages needed for session
    neededPackages = c("dplyr", "tidyr", "ggplot2", 
                       "magrittr","infuser", "countrycode")
    allPackages    = c(neededPackages %in% installed.packages()[,"Package"])

    # Install packages (if not already installed)
    if (!all(allPackages)) {
        missingIDX = which(allPackages == FALSE)
        needed     = neededPackages[missingIDX]
        lapply(needed, install.packages)
    }

    # Load all defined packages
    lapply(neededPackages, function(x) suppressPackageStartupMessages(
        library(x, character.only = TRUE)))

    # Load dataset
    cat("Loading data set...", sep = "\n")
    load(dataset)
    
    dat.input = easySHARE_rel6_0_0
    rm(easySHARE_rel6_0_0)
    
    rows.dat.input = nrow(dat.input)

    ############################################################################
    # ENCODE MISSING VALUES

    # Organize data.frame by selecting relevant variables
    cat(infuse("Selecting values only from Wave {{wav}} and between ages 50 and 64.", wav = wav), sep = "\n")
    
    dat = dat.input %>%
        dplyr::filter(wave == wav & (age <= 64 & age >= 50)) %>% 
        dplyr::select(wave, country_mod,                 # dataset details
               female, age, isced1997_r, ch001_, mar_stat, # demo variables
               chronic_mod, maxgrip, adla, bmi, bmi2, eurod, sphus, # health
               ep013_mod, ep005_)                          # labor (outcome var)
    
    rm(dat.input)

    # Encode missing values according to SHARE dataset guidelines
    a = c(-2, -3, -4, -7, -9, -12, -13, -14, -15, -16)
    b = c("tocheck","implausible", "tocheck", "uncoded", "notApplicable",
          "dontKnow", "notAskedWave", "notAskedCountry", "noInformation",
          "noDropOff")
    missing.value.codes = data.frame(a,b)
    # This data frame can be used to verify NA codes easily.
    # But for the encoding only the numeric vector a is necessary.

    # Find NA locations and declare them as such
    df.decl = apply(dat, 2, function(z) {
        na.loc    = which(z %in% a)
        z[na.loc] = NA
        return(z)
    })

    df = data.frame(df.decl)

    # If working hours is NA and job status is not employed, this means 
    # individuals don't work
    # If working hours is NA and job status is employed, we assume part time 
    # work
    df$ep013_mod[is.na(df$ep013_mod) & df$ep005_ != 2] = 0
    df$ep013_mod[is.na(df$ep013_mod) & df$ep005_ == 2] = 1

    ############################################################################
    # CREATE DATA FRAMES FOR ANALYSIS AND ESTIMATION

    # Get country information from "countrycode" package
    country_list = c("BEL", "NLD", "FRA", "SWE", "DEU", "GRC", "ITA", "ESP", 
                     "DNK", "AUT", "CHE")
    country_data = with(countrycode_data, 
                        data.frame(iso3c, iso3n, country.name.en))

    # Variables are cleaned, converted into human-readable naming conventions, 
    # and converted to dummies as described in the paper
    cat("Removing missing values.", sep = "\n")
    
    df.out       = df %>%
        dplyr::left_join(country_data, by = c("country_mod" = "iso3n")) %>%
        dplyr::filter(iso3c %in% country_list) %>%
        dplyr::mutate(country       = factor(country.name.en),
                      gender        = factor(ifelse(female, "FEMALE", "MALE")),
                      age50_54      = age < 55,
                      age55_59      = age >= 55 & age < 60,
                      age60_64      = age >= 60,
                      age           = factor(floor(age)),
                      edu_low       = isced1997_r %in% c(0, 1),
                      edu_second    = isced1997_r %in% 2:4,
                      edu_high      = isced1997_r %in% c(5, 6),
                      children      = ch001_,
                      couple        = mar_stat %in% 1:3,
                      h_chronic     = chronic_mod,
                      h_maxgrip     = maxgrip,
                      h_adla        = adla > 0,
                      h_overweight  = bmi2 == 3,
                      h_obese       = bmi2 == 4,
                      h_badmental   = eurod > 3,
                      h_goodsp      = sphus < 4,
                      h_bmi         = floor(bmi),
                      h_depression  = eurod,
                      h_perceived   = sphus,
                      labor_ft      = ep013_mod >= 32,
                      labor_pt      = ep013_mod < 32 & ep013_mod > 0,
                      labor_np      = ep013_mod == 0,
                      labor_hrs     = floor(ep013_mod)) %>%
        dplyr::select(country, gender,              # country and gender
                      starts_with("age"),           # age dummy
                      starts_with("h_"),            # health indicators
                      starts_with("edu_"),          # eduction dummies
                      children, couple,             # demographic details
                      starts_with("labor_")) %>%    # labor supply outcomes
        na.omit() %>%                               # remove missing values
        set_rownames(NULL)                          # reset row numbering

    # TODO: determine threshold for "severe" and "mild" conditions (currently we
    # just set it as numeric

    rows.df.out    = nrow(df.out)
    rows.remove    = rows.dat.input - rows.df.out
    
    cat(infuse("Rows removed:   {{rows.remove}}", rows.remove = rows.remove), 
        sep = "\n")
    cat(infuse("Rows remaining: {{rows.dat}}", rows.dat = rows.df.out), 
        sep = "\n")
    cat("", sep = "\n")

    # Create standardized variables for numeric data
    standardize = function(x) {
        mean = sum(x)/length(x)
        std  = sd(x)
        val  = (x - mean) / std
        return(val)
    }
    
    # Standardize and select correct variables
    standardize.df = function(df) {
        # Gives a vector of integer column positions of numeric variables
        idx = sapply(df, is.numeric)
        idx = seq(1:length(idx))[idx]
        
        # Creating separate data set with standardized numeric variables for 
        # regression, then reselect variables as described in paper (e.g. self-           # reported health is removed)
        df.reg = df %>%
            mutate_at(.vars = vars(idx),
                      .funs = standardize) %>%
            mutate(labor_participation = !labor_np) %>% # invert to get labor_pt
            dplyr::select(country, gender, age,
                          h_chronic, h_adla, h_obese, h_maxgrip,
                          edu_second, edu_high, children, couple,
                          labor_participation)
        
        return(df.reg)
    }
    
    # Split data frames into country/gender splits, then standardize numeric
    splits    = split(df.out, f = list(df.out$country, df.out$gender), 
                      drop = TRUE)
    df.reg    = standardize.df(df.out)
    df.splits = lapply(splits, standardize.df)

    # Create necessary dummary variables for regression
    dummify = function(df) {
        df = df %>%
            dplyr::select(-country, -gender)        # remove country/gender
        model      = ~ 0 + .                        # needed to remove intercept
        new.df     = model.matrix(model, df)# create dummies
        new.df     = data.frame(new.df)
        return(new.df)
    }

    df.splits = lapply(df.splits, dummify)

    # df.out is for analysis, df.reg and df.splits are for estimation
    all.output = list(df.out = df.out, df.reg = df.reg, df.splits = df.splits)
    
    cat("Final output is a list containing 3 data.frames:", sep = "\n")
    cat("`df.out` is a data.frame containing variables for calculating summary statistics.", sep = "\n")
    cat("`df.reg` is a data.frame containing standardized variables for regression.", sep = "\n")
    cat("`df.splits` splits the regression data.frame into individual data.frames for each country/gender split. 22 data.frames are contained in this list.", sep = "\n")

    return(all.output)
}
