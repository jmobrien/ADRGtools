dupInfo <-
  function(x, useNA="ifany"){
    # this looks for duplicates
    dupdat <- data.frame(table(x, useNA = useNA))
    dupdat <- dupdat[dupdat$Freq > 1,]
    dupdat$position <-
      lapply(dupdat$x, function(y){which(x %in% y)})
    dupdat
  }

construct.all <- function(...){
  # construct.all() Forms string elements (often, variable names), of all
  # combinations of a set of characteristics. For instance,
  # construct.all(c("algebra.", "calculus.", "geometry"), 1:4, "_", letters[1:4])
  # would make a vector 3x4x1x4 of format "[mathtype].[1-4]_[a-d]."  Useful when
  # constructing a list of various categories of similarly named variables.
  vals <- 
    apply(
      expand.grid(..., stringsAsFactors = FALSE),
      1,
      paste,
      collapse = ""
    )
  vals
}


nameslike <- function(data, regexp){
  # shortcut for pulling out similarly named variables--returns all vars that have
  # a particular string or regular expression (really just a shortcut to save
  # typing)
  grep(regexp, names(data), value=TRUE)
}


# %!in% - a convenience function for "not in" 
`%!in%` <- Negate("%in%")

# Allows you to simultaneously extract and rename variables from a data frame
# Uses the 
select.rename <- function(df, ...){
     df %>% select(!!!rlang::syms(...))
}

