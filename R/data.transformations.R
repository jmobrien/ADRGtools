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


OutRemove <- function(x){
# Outlier Removal
# For all values that are ≥ 3sd than the mean (post-transformation in that case) recode values to the second highest amount:
  recode <- which(x >=  (mean(x) + 3*sd(x)))
  x2 <- x
  if(length(recode) != 0L) {x2[recode] <- max(x[-recode])}
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
