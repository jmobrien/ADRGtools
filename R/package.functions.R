get.packages <- function(...){
  packages <- list(...)
  invisible(
  lapply(packages, function(x){
    if(!require(x, character.only = TRUE)){install.packages(x);require(x, character.only = TRUE)}
  }))}

