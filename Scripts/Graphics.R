################################################################################
# Graphics.R
#
################################################################################
# Description:
# Functions designed to create useful graphics (quantlet 6)
# 
# xxxx
# 
# health.distribution(xvar, gen, countries, remove.outliers): show distribution
# of numeric variable in dataset, facetted by gender and country.
# Select a numeric variable in xvar (as a string), a vector of genders, and a
# vector of countries. By default, outliers are removed.
# 
################################################################################

###### LABOR PARTICIPATION RATES ACROSS SEGMENTS
# Chloropeth of labor participation rates (men & women) (across age groups) - 6 total graphs. ALLOW FOR SELECTION OF T/F variable

###### VIEW DISTRIBUTION OF NUMERIC VARIABLES
# Numeric variable: shows distribution for each country, then overlays average of entire data set 

# slicing by dummy variables is not included. that can be done before the function is called and then that df can be paseed to this

# by default, gender is "all". can either be "all", "MALE" or "FEMALE"
# by default, countries is "all". can also pass a character vector of countries

# TODO: Add option for no gen/country split?
# TODO: Should the entire distribution be overlaid or by gender?

health.distribution = function(xvar, 
                               gen              = "all", 
                               countries        = "all", 
                               remove.outliers  = TRUE) {
    
    # first convert age to numeric, not factor variable
    df.out$age = as.numeric(levels(df.out$age)[df.out$age])
    
    # STOPPING CONDITIONS
    if (!is.numeric(df.out[[xvar]])) stop("'xvar' must be numeric")
    if (length(gen) > 1 | !all(gen %in% c("all", "MALE", "FEMALE"))) {
        stop("'gender' must be one of 'all', 'MALE', or 'FEMALE'")
    }
    if (length(countries) > 1 & !all(countries %in% levels(df.out$country))) {
        stop("'countries' must be 'all' or a character vector containing countries in the `df.out` data frame")
    }
    if (length(countries) == 1) {
        if (!(countries %in% c('all', levels(df.out$country)))) {
            stop("'countries' must be 'all' or a character vector containing countries in the `df.out` data frame")
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

###### VIEW LABOR CHOICE (AS DIVERGING STACKED BAR CHART)
# Decide best graph to show. Could be stacked bar, could be separate bars, could be diveging (like a Likert scale)

# I like the idea of "tranching" people into categories based on health outcomes. Maybe we'll see something like how really healthy people have amazing labor rates, but unhealthy people have super garbage rates. And that maybe incremental gains from the unhealthy population will lead to higher participation rates, etc. (How to influence policy!) - inequality of health. For that we'll need to run regression on separate buckets of health outcomes