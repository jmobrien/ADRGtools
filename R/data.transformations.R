

# zScore - class-preserving standardization function ----------------------

zScore  <- function(x){
  # z-scoring that preserves data type (scale() converts to matrix)  
  as.numeric(scale(x))}


zScoreLong <- function(data, x, group){
  # does z-scoring by group for L2 variables, where they are repeated across rows in a dataset
  if(any(eval(substitute(aggregate(x ~ group, data = data, FUN = var, na.rm=T)))[[2]] != 0))
    stop("Varable is not equal within some groups")
  vals <- eval(substitute(aggregate(x ~ group, data = data, FUN = unique)))
  vals$zvals <- zScore(vals[[2]])
  vals[match(eval(substitute(data$group)), vals[[1]]), 3]
}

# Does within-group zscoring:
zScoreGroup <- function(data, x, group){
  eval(substitute(ave(data$x, as.factor(group), zScore)))
}

# range0to1 - maps variable to [0,1] --------------------------------------

range0to1 <- 
  function(
    x, # variable that defines the range that will be mapped to [0,1]
    min=NULL, # value to be mapped to 0, will use variable min if not specified
    max=NULL,  # value to be mapped to 1, will use variable max if not specified
    newdata = NULL # Calculate a different variable on the range of the specified one
    ){
  #function for making the range of a function 0 to 1 while maintaining scaling
  if(is.null(min)){
    min <- min(x, na.rm=T)
    cat(paste0("\nObserved minimum value is ", min, "\n\n"))
    }
  if(is.null(max)){max <- max(x, na.rm=T)
  cat(paste0("\nObserved maximum value is ", max, "\n\n"))
  }
  if(!is.null(newdata)){  
    (newdata-min)/(max-min)
  } else {
    (x-min)/(max-min)
  }  
}  


# revCode - reverse codes variables ---------------------------------------


revCode <- 
  function(x, 
           max = NULL, # maximum possible value, will use variable's max if not specified
           min = NULL # minimum possible value, will use variable's min if not specified
           ){
  # Function for reverse coding
  if(is.null(min)){min <- min(x, na.rm=T)}
  if(is.null(max)){max <- max(x, na.rm=T)}
  max + min - x
  }


# OutRemove - Checks for and Winsorizes outliers --------------------------


OutRemove <- 
  function(
    x, # variable to trim
    direction = "both", # Direction for one-tailed test
    dev.cutoff = 3 # cutoff in standard deviation units
    ){
    # Outlier Removal
    # recode values to the first non-outlier value:
    if(!direction %in% c("upper", "Upper", "UPPER", "lower", "Lower", "LOWER", "both", "Both", "BOTH"))
      stop("Direction should be 'upper' (positive outliers), 'lower' (negative outliers) or 'both' (two-tailed)") 

    dev <- (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
    x2 <- x
    # Recode lower tail
    if(direction %in% c("both", "Both", "BOTH", "lower", "Lower", "LOWER"))
      x2[dev < -dev.cutoff] <- min(x[dev >= -dev.cutoff], na.rm=T)
    # Recode upper tail
    if(direction %in% c("both", "Both", "BOTH", "upper", "Upper", "UPPER"))
      x2[dev > dev.cutoff] <- max(x[dev <= dev.cutoff], na.rm=T)
    x2
  }




ZandTrim <- 
  function(
    dat, # a dataframe
    vars, # a vector of variables, in quotes
    direction = "both", # Direction for one-tailed test
    dev.cutoff = 3 # cutoff in standard deviation units
  ) {
    # convenience function - 
    # trims all variables in named vector (defaults to both tails, 3sd cutoff,
    # does z-scoring of the variables trimmed and untrimmed,
    # outputs the final product as a new dataframe including the added items.
    # Gives warning if some varibles don't have trimmable values.
    
    if(!all(c(vars) %in% names(dat))){
      missings <- c(vars)[!c(vars) %in% names(dat)]
      stop(paste0("Variables ", paste0(missings, collapse = ", "), " not in data"))
    }
    
    # create new variable names:
    vars.z <- paste0(vars, ".z")
    vars.trim <- paste0(vars, ".trim")
    vars.trim.z <- paste0(vars.trim, ".z")

    # add to data frame:
    dat[vars.z] <- 
      lapply(dat[vars], zScore)
    dat[vars.trim] <- 
      lapply(dat[vars], OutRemove, direction=direction, dev.cutoff=dev.cutoff)
    dat[vars.trim.z] <- 
      lapply(dat[vars.z], OutRemove, direction=direction, dev.cutoff=dev.cutoff)
 
    # Check for outlier status, delete variables that don't have outliers:
    trimtest <- 
      vapply(seq_along(vars), function(i){
        all(dat[vars[i]] == dat[vars.trim[i]], na.rm=T)
    }, FUN.VALUE = T)
    dat[vars.trim[trimtest]] <- NULL
    dat[vars.trim.z[trimtest]] <- NULL
    
    if(any(trimtest)) {
      warning(
        paste0("Variables ", paste(vars[trimtest], collapse = ", "), " have no outliers; no trimmed equivalents will be created for them.")
      )}
    
    # return the new dataframe:
    dat
    
  }  
