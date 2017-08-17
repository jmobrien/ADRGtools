
# This function allows you to load numerous packages and install them if they aren't there.  
# You just have to write get.packages("car", "ggplot2", "lme4", etc.)
get.packages <- function(...){
  packages <- list(...)
  invisible(
    lapply(packages, function(x){
      if(!require(x, character.only = TRUE)){
        install.packages(x);require(x, character.only = TRUE)
      }}))}

