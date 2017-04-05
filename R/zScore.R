# z-scoring that preserves data type (scale converts to matrix)
zScore  <- function(x){as.numeric(scale(x))}


# a function for "not in" - doesn't load from the package yet, but you can copy/paste it.
"%!in%" <- Negate("%in%")

# reverse code
revCode <- function(x, max_x, min_x){max_x + min_x - x}

makevar <- 
  function(
    data, # data for use
    vars, # Non-reverse coded variables (vector in quotes)
    rev.vars = NULL, #Reverse coded variables (vector in quotes)
    rev.max = NULL, # Maximum value of scale (used for reverse coding)
    rev.min = 1 #minimum amount of scale (used for reverse coding, assumed 1)
  ) {
    # Data frame for making means:
    ourdat <- data[vars]
    
    # Reverse coding if necessary:
    if(!is.null(rev.vars)){
      
      ourdat[rev.vars] <-
        lapply(data[rev.vars],
               function(rvar){
                 rev.max-rvar+rev.min
               })
      
    }
    
    rowMeans(ourdat, na.rm=TRUE)
  }  

# Forms string elements (often, variable names), of all combinations of a set of characteristics.
# For instance, construct.all(c("algebra.", "calculus.", "geometry"), 1:4, "_", letters[1:4]) would
# make a vector 3x4x1x4 of format "[mathtype].[1-4]_[a-d]"

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

# shortcut for pulling out classes of names of variables, saving you the trouble of typing
nameslike <- function(data, regexp){
  grep(regexp, names(data), value=TRUE)
}

groupcenter <- 
  function(dat, grouping){
    g.mean <- 
      ave(dat, 
          as.factor(grouping),
          FUN = function(x){mean(x, na.rm=T)}
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
