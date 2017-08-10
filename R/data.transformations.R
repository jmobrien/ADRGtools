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

# Make L1 and L2 variables and standardize (NOT done yet)

# make.multilevel.vars <- 
#   function(dat, groupvar, vars){
#     
#     # Make formulae
#     formulae <- 
#       lapply(vars, function(x){
#      as.formula(paste0(x, " ~ 1 + (1 |", groupvar, ")"))
#       }
#     
#     # Get coefficients
#     coefs <- 
#     lapply(formulae, function(x){
#       data.framecoef(lmer(x, dat))}
#     
#            
#     lengths <- lapply(coefs, function(x){length(x[[groupvar]]$`(Intercept)`)} 
#     
#     as.vector(
#       lapply(
#         coefs, 
