# z-scoring that preserves data type (scale() converts to matrix)  
zScore  <- function(x){
  # z-scoring that preserves data type (scale() converts to matrix)  
  as.numeric(scale(x))}



#function for making the range of a function 0 to 1 while maintaining scaling
range0to1 <- function(x){
  #function for making the range of a function 0 to 1 while maintaining scaling
  (x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T))
}  

# Function for reverse coding
revCode <- function(x, max_x, min_x){
  # Function for reverse coding
  max_x + min_x - x}

 



# Group centering data- give data and grouping variable
groupcenter <- 
  function(dat, grouping, na.rm=TRUE){
    g.mean <- 
      ave(dat, 
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
