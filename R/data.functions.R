
# makevar - tool for quick compositing w/reverse coding & adjustme --------




# Function that can handle reverse coded items when making composites
makevar <- 
  function(
    dat, # data for use
    vars, # Non-reverse coded variables (vector in quotes)
    rev.vars = NULL, #Reverse coded variables (vector in quotes)
    rev.max = NULL, # Maximum value of scale (used for reverse coding)
    rev.min = 1, #minimum amount of scale (used for reverse coding, assumed 1)
    adjust = NULL, # numeric adjustment (will be added to each variable)
    type = "mean" # mean or sum
  ) {
    # Function that can quickly consruct composites, handling reverse
    # coding as needed, and can provide overall up/down adjustment (for example,
    # a clinical scale measured on 1-3 that is supposed to be scored 0-2.

    if(!all(c(vars, rev.vars) %in% names(dat))){
      missings <- c(vars, rev.vars)[!c(vars, rev.vars) %in% names(dat)]
      stop(paste0("Variables ", paste0(missings, collapse = ", "), " not in data"))
    }
    
    if(!type %in% c("mean", "sum", "MEAN", "SUM", "Mean", "Sum")){
      stop("type must be 'mean' or 'sum'")
    }
    
    # Data frame for making means:
    ourdat <- dat[vars]
    
    # Reverse coding if necessary:
    if(!is.null(rev.vars)){
      
      if(is.null(rev.max)){
        rev.max <- max(dat[rev.vars], na.rm = T)
        message(sprintf("For reversed items, estimated maximum as %d, minimum as %d", rev.max, rev.min))
      }
      
      ourdat[rev.vars] <-
        lapply(dat[rev.vars],
               function(rvar){
                 rev.max - rvar + rev.min
               })
      
    }
    
    # Adjustments as necessary
    if(!is.null(adjust)){
      ourdat <- ourdat + adjust
    }
  
      
  if(type %in% c("mean", "Mean", "MEAN")){  
    #Calculate the mean composite index
    out <- 
     rowMeans(ourdat, na.rm=TRUE)
  } else { 
   out <- 
     # Does sums, giving NA for cases missing on all data:
     ifelse(apply(ourdat, 1, function(x){all(is.na(x))}),
     NA,
     rowSums(ourdat, na.rm=T)
     )
  }
    out
  }
    


# merge.with.order - row-order-preserving merge tool ----------------------



merge.with.order <- 
  function(x,y, ..., sort = T, keep_order){
    # this function works just like merge, only that it adds the option to return
    # the merged data.frame ordered by x (1) or by y (2) From
    # https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
    add.id.column.to.data <- function(DATA)
    {
      data.frame(DATA, id... = seq_len(nrow(DATA)))
    }
    # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
    order.by.id...and.remove.it <- function(DATA)
    {
      # gets in a data.frame with the "id..." column.  Orders by it and returns it
      if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
      
      ss_r <- order(DATA$id...)
      ss_c <- colnames(DATA) != "id..."
      DATA[ss_r, ss_c]
    }
    
    # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
    # tmp()
    
    if(!missing(keep_order))
    {
      if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
      if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
      # if you didn't get "return" by now - issue a warning.
      warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
    } else {return(merge(x=x,y=y,..., sort = sort))}
  }


# EBmeans - making Empirical Bayes means ----------------------------------


EBmeans <- 
  function(
    data, 
    variable, 
    grouping){
    # This function calculates empirical Bayesian means in a way that eliminates the 
    # need for a multistep process in code.
    
    # Run model:
    require(lme4)
    mod <- eval(substitute(lmer(variable ~ (1 | grouping), data)))
    # Extract coefficients:
  coefs <- data.frame(
    grouping = rownames(coef(mod)[[1]]),
    coefs = coef(mod)[[1]]$`(Intercept)`,
    stringsAsFactors = FALSE
  )
  # Mix it in with the data:
  coefs.full <- merge(
    data.frame(
      index = 1:nrow(data),
      grouping = eval(substitute(data$grouping)), stringsAsFactors = FALSE),
    coefs,
    all.x = TRUE
  )
  #Output the EB mean values:
  coefs.full[["coefs"]][order(coefs.full["index"])]
}



# ControlFor - construct residual variables  --------



ControlFor <-function(
  dat, # dataframe, not in quotes
  outcome, # outcome variable, in quotes
  ... # list of variables, comma separated, in quotes
  ){
  # will create a variable that is controlled for any number of other variables.
  cvars <- unlist(list(...))
  form <- as.formula(paste0(outcome, " ~ ", paste0(cvars, collapse = " + ")))
  resid(lm(form, dat, na.action="na.exclude"))
}



# Standard error ----------------------------------------------------------

se <- function(
  vec # numeric vector
  ){
  sd(x, na.rm = TRUE) / sqrt(length(x))
}
  
