################################################################################
# SPL_ELP_Graphics.R
#
################################################################################
# Description:
# Functions designed to create useful graphics (quantlet 6)
# Note that these functions are designed to specifically run with the 'df.out' 
# data frame in the datasets lists found in `easySHARE_clean.RData`
# 
# health.gridmap(xvar, facetting): generate a tile grid map by a variable in
# dataset, facetted by either gender or age. if numeric than average is
# calculated. if logical than % is calculated. both xvar and facetting should be
# character vectors. returns a grid object, which can be printed to device with # grid.draw() or saved with ggsave()
# 
# health.distribution(xvar, gen, countries, remove.outliers): show distribution
# of numeric variable in dataset, facetted by gender and country.
# Select a numeric variable in xvar (as a string), a vector of genders, and a
# vector of countries. By default, outliers are removed.
# 
################################################################################

###### GEOGRAPHIC VISUALIZATION OF NUMERIC DATA
# Tile grid map of numeric variables by geographic location

# accept metric and facetting as input. xvar is a character vector
# facetting can take either 'gender' or 'age' as inputs
# return error if neither 'gender' or 'age' is inputted

# output side by side grid maps
health.gridmap = function(xvar, facetting) {
    
    # List all packages needed for session
    neededPackages = c("dplyr", "ggplot2", "countrycode", 
                       "gridExtra", "grid", "infuser")
    allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 
    
    # Install packages (if not already installed) 
    if(!all(allPackages)) {
        missingIDX = which(allPackages == FALSE)
        needed     = neededPackages[missingIDX]
        lapply(needed, install.packages)
    }
    
    # Load all defined packages (silently)
    invisible(lapply(neededPackages, library, character.only = TRUE))
    
    # first convert age to numeric, not factor variable
    if (is.factor(df.out$age)) {
        df.out$age = as.numeric(levels(df.out$age)[df.out$age])
    }
    
    # STOPPING CONDITIONS
    if (!is.numeric(df.out[[xvar]]) & !is.logical(df.out[[xvar]])) {
        stop("'xvar' must be numeric 
        or logical")
    }
    if (!(facetting %in% c('gender', 'age'))) {
        stop("'facetting' variable 
        must be either 'gender' or 'age'")
    }
    # ADD TILE GRID LAYOUT
    coordinates = structure(list(
        Country = c("Albania", "Austria", "Belarus", "Belgium", 
                    "Bosnia and Herzegovina", "Bulgaria", "Croatia", 
                    "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                    "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                    "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
                    "Montenegro", "Netherlands", "Norway", "Poland", "Portugal",
                    "Republic of Moldova", "Romania", "Russian Federation", 
                    "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
                    "Switzerland", "The former Yugoslav Republic of Macedonia", 
                    "Ukraine", 
                    "United Kingdom of Great Britain and Northern Ireland"
                    ), 
        X = c(6, 5, 7, 3, 6, 8, 5, 5, 5, 7, 7, 2, 5, 7, 6, 1, 1, 4, 8, 7, 3, 3,
              6, 4, 5, 6, 1, 8, 7, 9, 7, 6, 4, 2, 6, 4, 8, 7, 2
              ), 
        Y = c(1, 4, 6, 6, 3, 3, 3, 5, 7, 8, 9, 5, 6, 1, 4, 9, 7, 4, 7, 7, 5, 3,
              2, 6, 9, 6, 4, 4, 4, 7, 3, 5, 3, 4, 9, 5, 2, 5, 7
              ), 
        InSet = c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,
                  FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE,
                  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
                  FALSE, FALSE, FALSE
                  )
        ), 
        .Names = c("Country", "X", "Y", "InSet"), 
        row.names = c(NA, -39L), 
        class = c("tbl_df", "tbl", "data.frame")
    )
    
    # Get correct country information
    codes       = countrycode_data
    df.codes    = left_join(coordinates, codes, 
                            by = c("Country" = "country.name.en")) %>% 
        dplyr::select(Country, iso3c, X, Y) 
    
    # plot function to be used later
    plot_function = function(df) {
        
        p = ggplot(data = df, aes(x = X, y = Y)) +
            geom_tile(aes(fill = get(xvar), color = ""), 
                      color = "white", size = 0.6) +
            geom_text(aes(label = iso3c), color = "white") +
            geom_text(aes(label = round(get(xvar), 2)), vjust = 2, 
                      color = "white", size = 3, na.rm = TRUE) +
            coord_fixed(ratio = 1) +
            theme_minimal() +
            theme(axis.line        = element_blank(),
                  axis.text        = element_blank(),
                  axis.title       = element_blank(),
                  panel.background = element_blank(),
                  panel.grid       = element_blank(),
                  legend.position  = "bottom") +
            theme(plot.title    = element_text(size  = 16),
                  plot.subtitle = element_text(size  = 10, 
                                               color = "#7F7F7F"))
        return(p)
    }
    
    # IF GENDER SELECTED:
    if (facetting == 'gender') {
        
        # if logical than calculate percentage
        if (is.logical(df.out[[xvar]])) {
            
            var.type = '% with'
            
            df.graph = df.out %>% 
                dplyr::select(country, gender, `xvar`) %>% 
                group_by(country, gender) %>% 
                summarize(value = sum(get(xvar))/length(get(xvar)))
            names(df.graph)[3] = xvar
            
            # split by gender
            df.m = df.graph %>% 
                filter(gender == "MALE")
            df.f = df.graph %>% 
                filter(gender == "FEMALE")
            df.m = suppressWarnings(
                left_join(df.codes, df.m, c('Country' = 'country'))) %>%
                dplyr::select(iso3c, X, Y, `xvar`)
            df.f = suppressWarnings(
                left_join(df.codes, df.f, c('Country' = 'country'))) %>%
                dplyr::select(iso3c, X, Y, `xvar`)
        }
        
        # if numeric than calculate average
        if (is.numeric(df.out[[xvar]])) {
            
            var.type = 'Avg.'
            
            df.graph = df.out %>% 
                dplyr::select(country, gender, `xvar`) %>% 
                group_by(country, gender) %>% 
                summarize(value = mean(get(xvar)))
            names(df.graph)[3] = xvar
            
            # split by gender
            df.m = df.graph %>% 
                filter(gender == "MALE")
            df.f = df.graph %>% 
                filter(gender == "FEMALE")
            df.m = suppressWarnings(
                left_join(df.codes, df.m, c('Country' = 'country'))) %>%
                dplyr::select(iso3c, X, Y, `xvar`)
            df.f = suppressWarnings(
                left_join(df.codes, df.f, c('Country' = 'country'))) %>%
                dplyr::select(iso3c, X, Y, `xvar`)
        }
        
        # set equal range for color scales
        minval = min(df.f[[xvar]], df.m[[xvar]], na.rm = TRUE)
        maxval = max(df.f[[xvar]], df.m[[xvar]], na.rm = TRUE)
        
        # PLOT
        output = lapply(list(df.m, df.f), plot_function)
        output[[1]] = output[[1]] + 
            labs(title    = infuse("{{intro}} {{metric}} for {{split}}", 
                                   intro = var.type, metric = xvar, 
                                   split = 'male'),
                 fill     = xvar) + 
            scale_fill_continuous(low = "#fdd0a2", high = "#8c2d04", 
                                  na.value = "#CCCCCC", 
                                  limits = c(minval, maxval))
        output[[2]] = output[[2]] + 
            labs(title    = infuse("{{intro}} {{metric}} for {{split}}", 
                                   intro = var.type, metric = xvar, 
                                   split = 'female'),
                 fill     = xvar) + 
            scale_fill_continuous(low = "#fdd0a2", high = "#8c2d04", 
                                  na.value = "#CCCCCC", 
                                  limits = c(minval, maxval))
        return(arrangeGrob(output[[1]], output[[2]], ncol = 2))
    }
    
    # IF AGE SELECTED:
    if (facetting == 'age') {
        
        # if logical than calculate percentage
        if (is.logical(df.out[[xvar]])) {
            
            var.type = '% with'
            
            # split by age group
            df50 = df.out %>% 
                filter(age50_54 == TRUE) %>% 
                group_by(country) %>% 
                summarize(value = sum(get(xvar))/length(get(xvar)))
            df50 = suppressWarnings(
                right_join(df50, df.codes, c('country' = 'Country'))) %>% 
                dplyr::select(iso3c, X, Y, value)
            names(df50)[4] = xvar
            
            df55 = df.out %>% 
                filter(age55_59 == TRUE) %>% 
                group_by(country) %>% 
                summarize(value = sum(get(xvar))/length(get(xvar)))
            df55 = suppressWarnings(
                right_join(df55, df.codes, c('country' = 'Country'))) %>% 
                dplyr::select(iso3c, X, Y, value)
            names(df55)[4] = xvar
            
            df60 = df.out %>% 
                filter(age60_64 == TRUE) %>% 
                group_by(country) %>% 
                summarize(value = sum(get(xvar))/length(get(xvar)))
            df60 = suppressWarnings(
                right_join(df60, df.codes, c('country' = 'Country'))) %>% 
                dplyr::select(iso3c, X, Y, value)
            names(df60)[4] = xvar
        }
        
        # if numeric than calculate average
        if (is.numeric(df.out[[xvar]])) {
            
            var.type = 'Avg.'
            
            # split by age group
            df50 = df.out %>% 
                filter(age50_54 == TRUE) %>% 
                group_by(country) %>% 
                summarize(value = mean(get(xvar)))
            df50 = suppressWarnings(
                right_join(df50, df.codes, c('country' = 'Country'))) %>% 
                dplyr::select(iso3c, X, Y, value)
            names(df50)[4] = xvar
            
            df55 = df.out %>% 
                filter(age55_59 == TRUE) %>% 
                group_by(country) %>% 
                summarize(value = mean(get(xvar)))
            df55 = suppressWarnings(
                right_join(df55, df.codes, c('country' = 'Country'))) %>% 
                dplyr::select(iso3c, X, Y, value)
            names(df55)[4] = xvar
            
            df60 = df.out %>% 
                filter(age60_64 == TRUE) %>% 
                group_by(country) %>% 
                summarize(value = mean(get(xvar)))
            df60 = suppressWarnings(
                right_join(df60, df.codes, c('country' = 'Country'))) %>% 
                dplyr::select(iso3c, X, Y, value)
            names(df60)[4] = xvar
        }
        
        # set equal range for color scales
        minval = min(df50[[xvar]], df55[[xvar]], df60[[xvar]], na.rm = TRUE)
        maxval = max(df50[[xvar]], df55[[xvar]], df60[[xvar]], na.rm = TRUE)
        
        # PLOT
        output = lapply(list(df50, df55, df60), plot_function)
        output[[1]] = output[[1]] + 
            labs(title    = infuse("{{intro}} {{metric}} for {{split}}", 
                                   intro = var.type, metric = xvar, 
                                   split = 'age50_54'),
                 fill     = xvar) +
            scale_fill_continuous(low = "#fdd0a2", high = "#8c2d04", 
                                  na.value = "#CCCCCC", 
                                  limits = c(minval, maxval))
        output[[2]] = output[[2]] + 
            labs(title    = infuse("{{intro}} {{metric}} for {{split}}", 
                                   intro = var.type, metric = xvar, 
                                   split = 'age55_59'),
                 fill     = xvar) +
            scale_fill_continuous(low = "#fdd0a2", high = "#8c2d04", 
                                  na.value = "#CCCCCC", 
                                  limits = c(minval, maxval))
        output[[3]] = output[[3]] + 
            labs(title    = infuse("{{intro}} {{metric}} for {{split}}", 
                                   intro = var.type, metric = xvar, 
                                   split = 'age60_64'),
                 fill     = xvar) + 
            scale_fill_continuous(low = "#fdd0a2", high = "#8c2d04", 
                                  na.value = "#CCCCCC", 
                                  limits = c(minval, maxval))
       
        return(arrangeGrob(output[[1]], output[[2]], output[[3]], ncol = 3))
        
    }
}

    
###### VIEW DISTRIBUTION OF NUMERIC VARIABLES
# Numeric variable: shows distribution for each country, then overlays average of entire data set 

# slicing by dummy variables is not included. that can be done before the function is called and then that df can be paseed to this

# xvar is a character vector
# by default, gender is "all". can either be "all", "MALE" or "FEMALE"
# by default, countries is "all". can also pass a character vector of countries

health.distribution = function(xvar, 
                               gen              = "all", 
                               countries        = "all", 
                               remove.outliers  = TRUE) {
    
    # List all packages needed for session
    neededPackages = c("dplyr", "ggplot2", "scales")
    allPackages    = c(neededPackages %in% installed.packages()[ , "Package"]) 
    
    # Install packages (if not already installed) 
    if(!all(allPackages)) {
        missingIDX = which(allPackages == FALSE)
        needed     = neededPackages[missingIDX]
        lapply(needed, install.packages)
    }
    
    # Load all defined packages (silently)
    suppressMessages(lapply(neededPackages, library, character.only = TRUE))
    
    # first convert age to numeric, not factor variable
    if (is.factor(df.out$age)) {
        df.out$age = as.numeric(levels(df.out$age)[df.out$age])
    }
    
    # STOPPING CONDITIONS
    if (!is.numeric(df.out[[xvar]])) stop("'xvar' 
                                          must be numeric")
    if (length(gen) > 1 | !all(gen %in% c("all", "MALE", "FEMALE"))) {
        stop("'gender' must be one of 
             'all', 'MALE', or 'FEMALE'")
    }
    if (length(countries) > 1 & !all(countries %in% levels(df.out$country))) {
        stop("'countries' 
             must be 'all' or a character vector containing countries 
             in the `df.out` data frame")
    }
    if (length(countries) == 1) {
        if (!(countries %in% c('all', levels(df.out$country)))) {
            stop("'countries' 
                 must be 'all' or a character vector containing countries
                 in the `df.out` data frame")
        }
    }
    
    # Set faceting variables
    if (gen == "all") gen = c("MALE", "FEMALE")
    if (length(countries) == 1) {
        if (countries == "all") countries = levels(df.out$country)
    }
    
    # Filter countries/genders as selected
    df.out = df.out %>% 
        filter(country %in% countries, gender %in% gen)
    
    # Remove outliers (outside 1.5 IQR of 25% and 75% percentiles)
    rm.outliers = function(x, na.rm = TRUE) {
        qnt = quantile(df.out[[x]], probs = c(0.25, 0.75), na.rm = na.rm)
        out = 1.5*IQR(df.out[[x]], na.rm = na.rm)
        
        new.out = df.out
        new.out[(df.out[[x]] < qnt[1] - out), ] = NA
        new.out[(df.out[[x]] > qnt[2] + out), ] = NA

        new.out = new.out[complete.cases(new.out), ]
        return(new.out)
    }
    
    if (remove.outliers) df.out = rm.outliers(x = xvar)
    
    # Overlay lines for average over entire data set
    total.dist = table(df.out[[xvar]])/nrow(df.out)
    
    total        = expand.grid(countries, gen, sort(unique(df.out[[xvar]])))
    names(total) = c("country", "gender", xvar)
    total        = arrange(total, country, gender, get(xvar))
    total$value  = rep(total.dist, times = length(countries)*length(gen))

    plot.bar = ggplot(data = df.out, aes(x = get(xvar))) + 
        
        geom_bar(aes(fill  = country, 
                     color = country, 
                     y     = ..prop..), 
                 alpha = 0.4) + 
        
        geom_errorbar(data = total, aes(x    = get(xvar), 
                                        ymin = value, 
                                        ymax = value)) + 
        
        theme_minimal() +
        theme(legend.position = "none") + 
        theme(panel.grid.minor = element_blank()) + 
        theme(panel.grid.major = element_blank()) +
        theme(axis.text.y = element_blank()) +

        labs(title = paste0("Distribution of ", xvar, " by Country")) +
        labs(subtitle = "Faceted by country & gender. Distribution of the entire data set shown is overlaid on each graph.") +
        labs(x = xvar) + 
        labs(y = "") + 
        
        theme(plot.title = element_text(size=16)) +
        theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
    
    # show correct faceting and titles
    if (length(gen) == 1) {
        plot.bar = plot.bar + 
            labs(subtitle = paste0("Faceted by country. Gender selected is ", gen, ". Distribution of the entire data set shown is overlaid on each graph.")) + 
            facet_wrap(~ country, ncol = 4)
    } else {
        plot.bar = plot.bar + 
            facet_grid(gender ~ country)
    }
    
    return(plot.bar)
}
