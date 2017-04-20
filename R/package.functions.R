
# This function allows you to load packages and install them if they aren't there.  
# when you have it, you just have to write get.packages("packageA", "packageB", "packageC", ..., etc.)
get.packages <- function(...){
  packages <- list(...)
  invisible(
    lapply(packages, function(x){
      if(!require(x, character.only = TRUE)){install.packages(x);require(x, character.only = TRUE)}
    })
  )
}

