# z-scoring that preserves data type (scale() converts to matrix)  
zScore  <- function(x){
  # z-scoring that preserves data type (scale() converts to matrix)  
  as.numeric(scale(x))}



#function for making the range of a function 0 to 1 while maintaining scaling
range0to1 <- function(x, min=NULL, max=NULL){
  #function for making the range of a function 0 to 1 while maintaining scaling
  if(is.null(min)){min <- min(x, na.rm=T)}
  if(is.null(max)){max <- max(x, na.rm=T)}
  (x-min)/(max-min)
}  

# Function for reverse coding
revCode <- function(x, max_x=NULL, min_x=NULL){
  # Function for reverse coding
  if(is.null(min_x)){min_x <- min(x, na.rm=T)}
  if(is.null(max_x)){max_x <- max(x, na.rm=T)}
  max_x + min_x - x}


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



groupcenter <- 
# Group centering data- give data and grouping variable
  function(
    variable, # Variable to be group-centered/de-meaned
    grouping, # grouping variable
    na.rm=TRUE){
    g.mean <- 
      ave(variable, 
          as.factor(grouping),
          FUN = function(x){mean(x, na.rm=na.rm)}
      )
    dat - g.mean
  }

ZandTrim <- 
  function(
    dat, # a dataframe
    vars, # a vector of variables, in quotes
    direction = "both", # Direction for one-tailed test
    dev.cutoff = 3 # cutoff in standard deviation units
  ) {
    # convenience function, 
    # trims all variables in named vector (defaults to both tails, 3sd cutoff,
    # does z-scoring of the variables trimmed and untrimmed,
    # outputs the final product as a new dataframe including the added items.
    
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
