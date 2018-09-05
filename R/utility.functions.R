dupInfo <-
  function(x, useNA="ifany"){
    # this looks for duplicates
    dupdat <- data.frame(table(x, useNA = useNA))
    dupdat <- dupdat[dupdat$Freq > 1,]
    dupdat$position <-
      lapply(dupdat$x, function(y){which(x %in% y)})
    dupdat
  }

construct.all <- function(..., cycle.order = NULL){
  # construct.all() Forms string elements (often, variable names), of all
  # combinations of a set of characteristics. For instance,
  # construct.all(c("algebra.", "calculus.", "geometry"), 1:4, "_", letters[1:4])
  # would make a vector 3x4x1x4 of format "[mathtype].[1-4]_[a-d]."  Useful when
  # constructing a list of various categories of similarly named variables.
  if(!is.null(cycle.order)){
    if(!length(list(...)) == length(cycle.order) |
       !all.equal(sort(cycle.order), 1:length(cycle.order)))
       stop("bad order specification")
    
    #expand.grid cycles starting with the first column, so we change the order beforehand and use that.
    elems <- list(...)[cycle.order]
    
    elemgrid <- expand.grid(elems, stringsAsFactors = FALSE)[order(cycle.order)]
    
    vals <-
      apply(elemgrid, 1, paste, collapse = "")
  } else {
  
  vals <- 
    apply(
      expand.grid(..., stringsAsFactors = FALSE),
      1,
      paste,
      collapse = ""
    )
  }
  vals
}


nameslike <- function(data, regexp, case = TRUE){
  # shortcut for pulling out similarly named variables--returns all vars that have
  # a particular string or regular expression (really just a shortcut to save
  # typing)
  grep(regexp, names(data), value=TRUE, ignore.case = !case)
}


# %!in% - a convenience function for "not in" 
`%!in%` <- Negate("%in%")

# Allows you to simultaneously extract and rename variables from a data frame using quoted strings
# Uses a 
select.rename <- function(df, old, new, drop = TRUE){
  sel = as.list(setNames(old, new))
  if (drop == TRUE){
     out <- df %>% select(!!!rlang::syms(sel))
  }
  if (drop == FALSE){
     out <- df %>% rename(!!!rlang::syms(sel))
  }
  out
}

