
neededPackages = c("dplyr", "magrittr", "infuser", "countrycode", 
                   "dplyr", "tidyr", "magrittr", "purrr",
                   "formattable", "webshot", "htmltools", "webshot",
                   "aod", "devtools", "margins", "mfx",
                   "dplyr", "ggplot2", "countrycode", 
                   "gridExtra", "grid", "infuser")

pack = unique(neededPackages)

cite = lapply(pack, function(z){
  
  cc = try(citation(paste0(z)))
  
  if(class(cc) == 'try-error'){
    return(NULL)
  } else{
    return(cc)
    # n = paste0(z) # name
    # ccc = gsub(".*@Manual","",cc)
    # 
    # rdy = paste0("@Manual", ccc)
    # 
    # 
    # return(rdy)
    # 
    }
})


citeexists = Filter(Negate(is.null), cite)



sink(file = "packagereferences.bib", append = TRUE)

for(i in 1:length(citeexists)) print(citeexists[[i]])

sink()
