# this looks for duplicates
dupInfo <-
  function(x, useNA="ifany"){
  dupdat <- data.frame(table(x, useNA = useNA))
  dupdat <- dupdat[dupdat$Freq > 1,]
  dupdat$position <-
    lapply(dupdat$x, function(y){which(x %in% y)})
  dupdat
  }

# construct.all() Forms string elements (often, variable names), of all combinations of a set of characteristics.
# For instance, construct.all(c("algebra.", "calculus.", "geometry"), 1:4, "_", letters[1:4]) would
# make a vector 3x4x1x4 of format "[mathtype].[1-4]_[a-d]."  Useful when pulling in different classes
# of similarly named variables.
construct.all <- function(..., nse=FALSE){
  vals <- 
    apply(
    expand.grid(..., stringsAsFactors = FALSE),
    1,
    paste,
    collapse = ""
    )
  if (nse) {return(parse(text=vals))}
  vals
}

# shortcut for pulling out similarly named variables--returns all vars that have a particular string
# or regular expression (really just a shortcut to save typing)
nameslike <- function(data, regexp){
  grep(regexp, names(data), value=TRUE)
}


# a function for "not in" - doesn't load from the package yet, but you can copy/paste it.
"%!in%" <- Negate("%in%")