
# cl - linear model clustering function ----------------------------------------

# Included for use so that the specification curve can cluster standard errors if needed

cl   <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }



# Specification Curve Maker function --------------------------------------

s.curve <-
  function(
    #This function outputs a list of
    #TODO: make work with subsetting, possibly include nonstandard eval to make it easier to
    #      use interactively
    outcomes, # vector of outcome variables
    treatment, # vector of treatment variables
    cov.list, # named list, sets of covariates / moderators - write moderators as "var1:var2"
    # eg: cov.list = list(gender = c("gender.roster", "gender.selfreport"),
    
    no.cov.exclude = NULL, # Will add a model where each item of the list above is missing,
    # unless specified here
    extra.models = NULL, # Models written literally; will be appended to the set 
    # (still crossed against subsets and weights)
    dat, # a dataframe
    mod.type = lm, #takes lm, glm (NOT YET IMPLEMENTED)
    mod.family = NULL, # if family needed (NOT YET IMPLEMENTED)
    alpha = .05, # plausible alpha value (if one tailed, will test against p <= .1 on that side)
    tail = NULL, # One tailed test?  Takes "upper" and "lower"
    subsets = NULL, # runs subsets of data based on the specifications listed here (vector of conditions)
    subsets.exclude = TRUE, # if subsets added, includes an un-subsetted version
    weights = NULL, # vector of weight variables names to add to runs, re-named if desired
    weights.recalc = NULL, # Vector: function that can re-caculate the weights at each subset
    weights.exclude = TRUE, # Includes an unweighted version where weights added 
    permutations = NULL, # number of permutations for p-curve
    cluster = FALSE, # Cluster robust standard errors
    cluster.var = NULL, # Variable on which to cluster
    robust.se = NULL, # robustness adjustment-provide parameters in capital letters e.g. "HC1"
    cat.percent = TRUE, # displays summary output of % significant at the end of the data for convenience in interactive mode. Set to false if using as a part of an Rmarkdown file.
    keep.tidy.models = TRUE, # Set to false for large model samples
    keep.full.models = FALSE, # Set to false for large model samples
    model.only = FALSE, # Outputs the model for later use, rather than running
    perm.pvalues = FALSE # calculates permutation test-based pvalues (if permutation test active)
  ) {
    
    # Type of model:
    model.type <- deparse(substitute(mod.type))
    
    # Library calls ----
    
    
    require(broom)
    # require(ggplot2)
    
    # Initialize model list ----
    s.curve.mod <- 
      list(
        dat = dat,
        treatment = treatment,
        outcomes = outcomes,
        alpha = alpha,
        tail = tail,
        mod.type = model.type,
        mod.family = mod.family,
        cluster = cluster,
        cluster.var = cluster.var,
        robust.se = robust.se,
        permutations = permutations,
        keep.tidy.models = keep.tidy.models,
        keep.full.models = keep.full.models
      )
    
    # Construction of formulas ----
    
    # Create a vector of all included variables,
    # outcomes, treatment variables, and covariates:
    vars.list <- unique(c(outcomes, treatment, unlist(cov.list)))
    
    # Add NA's to beginning to model exclusion of that variable
    cov.list.na <-
      sapply(
        names(cov.list),
        function(x){
          if(x %in% no.cov.exclude){
            cov.list[[x]]
          } else {
            c(NA, cov.list[[x]])
          }
        },
        USE.NAMES = TRUE,
        simplify = FALSE
      )
    
    # Add in condition variables
    cov.list.final <-
      c(list(treatment = treatment), cov.list.na)
    
    # Expand to a data frame where each case
    # is elements to go in a formula right-hand side
    mod.grid <-
      expand.grid(cov.list.final, stringsAsFactors = FALSE)
    
    
    # Expand to a data frame where cases are full formulas,
    # including outcomes (for setting up element indexing later)
    mod.grid.all <-
      expand.grid(c(list(outcomes=outcomes), cov.list.final),
                  stringsAsFactors = FALSE)
    
    
    # Make right-hand side formula, dropping NA's, and adding "1"
    # to empty formula (to avoid errors)
    formulas.rhs <-
      apply(mod.grid, 1, function(x){
        formula.rhs <- paste(na.omit(x), collapse= " + ")
        formula.rhs[formula.rhs == ""] <- 1
        formula.rhs
      })
    
    # Probably don't want empty models, so throw warning:
    if( any(formulas.rhs == "1") ) {
      warning(
        "NOTE: set includes one or more null models."
      )}
    
    
    # Adds left-hand side (outcomes), make formulas:
    s.curve.mod$formulas <-
      lapply(
        as.vector(outer(outcomes, formulas.rhs,
                        paste, sep=" ~ ")),
        as.formula)
    
    if(!is.null(extra.models)){
      s.curve.mod$formulas <- 
        c(s.curve.mod$formulas, 
          lapply(extra.models, as.formula)
        )
    }
    # Converts formula names to character vector:
    s.curve.mod$formula.names <- 
      sapply(s.curve.mod$formulas, deparse, width.cutoff = 500)    
    
    # Length of formula list:
    s.curve.mod$n.formulas <- 
      length(s.curve.mod$formulas)
    
    # Initialize names of everything: 
    full.names <- s.curve.mod$formula.names
    
    # Make the data subsets:
    if (!is.null(subsets)){
      subsettings <-
        do.call(
          cbind.data.frame,
          lapply(subsets, function(singlesubset){
            with(dat, eval(parse(text = singlesubset)))
          }))
      
      subset.names <- paste0("SUBSET: ", subsets)
      names(subsettings) <- subset.names
      
      if (subsets.exclude) {
        subsettings <- 
          cbind.data.frame(
            setNames(data.frame(TRUE), "SUBSET: ALL DATA"),
            subsettings
          )
        
        subset.names <- names(subsettings)
      }
      
      
      # Update the full names
      full.names <-
        as.vector(outer(full.names, subset.names,
                        paste, sep=", "))
      
      s.curve.mod$n.subsettings <- length(subset.names)
      s.curve.mod$subsettings <- subsettings
      s.curve.mod$subset.names <- subset.names
    } else {
      # Just have a non-subsetting subset:
      subsettings <- setNames(NA, NA)
    }
    
    # Make the data weights:
    if (!is.null(weights)){
      # Make a vector of names for the weights used based on the variables:
      weights.names <- paste0("WEIGHT: ", weights)
      
      weightings <- 
        do.call(
          cbind.data.frame,
          lapply(weights, function(wset){
            dat[wset]
          }))
      
      names(weightings) <- weights.names          
      
      if (weights.exclude) {
        weightings <- 
          cbind.data.frame(
            setNames(data.frame(1), "WEIGHT: NONE"),
            weightings
          )
        weights.names <- names(weightings)
      }
      
      full.names <-
        as.vector(outer(full.names, weights.names,
                        paste, sep=", "))
      
      s.curve.mod$weightings <- weightings
      s.curve.mod$weights.names <- weights.names
      s.curve.mod$n.weightings <- length(weights.names)
      
    } else {
      weightings <- setNames(NA, NA)
    }
    
    s.curve.mod$n.specifications <- length(full.names)
    
    s.curve.mod$spec.list <- 
      setNames(
        expand.grid(
          s.curve.mod$formulas,
          names(weightings),
          names(subsettings),
          stringsAsFactors = FALSE
        ),
        c("formula", "weights", "subset")
      )
    
    s.curve.mod$spec.list$specification.index <- 
      seq_len(nrow(s.curve.mod$spec.list))
    
    if(model.only) return(s.curve.mod)
    
    
    
    
    # RUN MAIN SPECIFICATION CURVE ----
    s.curve.mod$results <-
      curveRunner(s.curve.mod)
    
    
    # Deal with Duplicates ----------------------------------------------------
    
    # Check for duplicated models, move to separate dataframe:
    duplicates <- 
      which(
        duplicated(
          lapply(s.curve.mod$results$final.specification, sort)
        ))
    
    s.curve.mod$has.duplicates <- FALSE
    
    if(length(duplicates) > 0){
      s.curve.mod$duplicated.models <-
        s.curve.mod$results[duplicates,]
      
      s.curve.mod$results <-
        s.curve.mod$results[-duplicates,]
      
      s.curve.mod$duplicate.specs <-
        s.curve.mod$spec.list[duplicates,]
      
      s.curve.mod$spec.list <-
        s.curve.mod$spec.list[-duplicates,]
      
      s.curve.mod$n.specifications.original <-
        s.curve.mod$n.specifications
      
      s.curve.mod$n.specifications <- 
        nrow(s.curve.mod$results)
      
      s.curve.mod$has.duplicates <- TRUE
      
      message("Duplicate models dropped, ", 
              s.curve.mod$n.specifications.original, 
              " now ",
              s.curve.mod$n.specifications,
              " (less ",
              s.curve.mod$n.specifications.original - 
                s.curve.mod$n.specifications,
              ")")
    }
    
    
    # RUN PERMUTATION TEST ----
    
    if(is.numeric(permutations)){
      
      # Save a nested list of individual data frames
      # like the one we just made above:
      perm.dat <- dat
      set.seed(2018)
      s.curve.mod$perm.test <-
        replicate(
          # number of permutations
          permutations,
          {
            # shuffle all treatment vars
            # Same ordering for all variables in a particular
            # iteration
            new.order <- sample(nrow(perm.dat))
            perm.dat[treatment] <-
              lapply(
                treatment,
                function(tvar){
                  perm.dat[[tvar]][new.order]
                })
            
            # run model for that permutation:
            curveRunner(s.curve.mod,
                        inc.full.models = FALSE,
                        inc.tidy.models = FALSE)
          },
          simplify=FALSE)
    } #
    
    # FINAL CALCULATIONS ----
    s.curve.update(s.curve.mod = s.curve.mod, 
                 perm.pvalues = perm.pvalues)
    
  }


# curveRunner - processes s-curve objects ---------------------------------

curveRunner <-
  function(
    s.curve.model,
    inc.tidy.models = NULL,
    inc.full.models = NULL,
    perm.index = NULL
  ){
    
    dat <- s.curve.model$dat
    spec.list <- s.curve.model$spec.list
    subsettings <- s.curve.model$subsettings
    weightings <- s.curve.model$weightings
    
    if(is.null(inc.tidy.models)){
      inc.tidy.models <- 
        s.curve.model$keep.tidy.models
    }
    
    if(is.null(inc.full.models)){
      inc.full.models <- 
        s.curve.model$keep.full.models
    }
    
    
    models.df <-
      lapply(seq_len(nrow(spec.list)), 
             # indexing function
             function(ind){
               
               # Initialize output:
               mod.row <- 
                 data.frame(
                   specification.no = spec.list$specification.index[ind],
                   model.type = 
                     paste0(s.curve.model$mod.type, "-", 
                            s.curve.model$mod.family, "-",
                            s.curve.model$robust.se),
                   stringsAsFactors = FALSE
                 ) 
               
               mod.row$formulas <-  
                 as.character(spec.list[["formula"]][ind])
               
               # Initialize model parameters:
               params <- 
                 list(
                   
                   # Add in data:
                   data = dat,
                   # Add in formulas:
                   formula = spec.list[["formula"]][[ind]]
                 )
               # Initialize final model report (for duplicate checking):
               final.model <- c()
               
               weights.name <- spec.list[ind, "weights"]
               subset.name <- spec.list[ind, "subset"]
               
               # Subsetting setup:
               if (!is.na(subset.name)){
                 # Establish the subset:
                 params$data <- dat[subsettings[[subset.name]],]
                 # note it in the output:
                 mod.row$subset.used <- subset.name
                 
                 # Update the final model report:
                 final.model <- c(final.model, subset.name)
               } 
               
               
               # Weighting setup:
               if (!is.na(weights.name)){
                 # Recalculate the weights variable:
                 if(weights.name != "WEIGHT: NONE"){
                   new.weight.table <- 
                     1/prop.table(
                       table(params$dat$treat_noun,
                             params$dat$ssi
                       ), margin = 2)
                   
                   params$weights <- 
                     diag(new.weight.table[
                       as.character(params$dat$treat_noun),
                       as.character(params$dat$ssi)
                       ])
                 }
                 # note it in the output:
                 mod.row$weights.used <- weights.name
                 
                 # Update the final model report:
                 final.model <- c(final.model, weights.name)
               }
               
               if(s.curve.model$mod.type == "glm"){
                 params$family <- s.curve.model$mod.family
               }
               
               FUN <- match.fun(s.curve.model$mod.type)
               
               model.run <- 
                 do.call(FUN, params)
               
               # Get out model features:
               model.glance <-
                 glance(model.run)
               
               
               # Clustering the model:
               if(s.curve.model$cluster) {
                 
                 # Load relevant packages:
                 require(sandwich, quietly = TRUE)
                 require(lmtest, quietly = TRUE)
                 
                 model.run.cluster <-
                   cl(dat = f.data, fm = model.run, cluster = f.data[[cluster.var]])
                 
                 model.tidy <-
                   tidy(model.run.cluster)
                 
                 
                 
               } else if(!is.null(s.curve.model$robust.se)) {
                 
                 # Load relevant packages:
                 require(sandwich, quietly = TRUE)
                 require(lmtest, quietly = TRUE)
                 
                 # Do standard error adjustment as called for by robust.se:
                 model.run.robust <- 
                   coeftest(model.run, 
                            vcov = vcovHC(model.run, 
                                          type=s.curve.model$robust.se)
                   )
                 
                 model.tidy <-
                   tidy(model.run.robust)
                 
                 
               } else {  
                 # If neither of these, just calculate from the main model: 
                 
                 # Gets tidied output (data frame) for concatenation:
                 model.tidy <-
                   tidy(model.run)
                 
                 
               }
               
               mod.row[c("estimate", "std.error", "statistic", "p.value")] <- 
                 model.tidy[model.tidy$term %in% s.curve.model$treatment, 
                            c("estimate", "std.error", "statistic", "p.value")]
               
               mod.row$significant <- 
                 if (is.null(s.curve.model$tail)){
                   mod.row$p.value <= s.curve.model$alpha
                 } else if (s.curve.model$tail == "upper") {
                   mod.row$p.value <= 2*s.curve.model$alpha &&
                     mod.row$estimate > 0
                 } else {
                   mod.row$p.value <= 2*s.curve.model$alpha &&
                     mod.row$estimate < 0
                 }
               
               # Model characteristics
               
               mod.row <- cbind.data.frame(mod.row, model.glance)
               
               # Model N
               if(s.curve.model$mod.type == "glm"){
                 mod.row["n"] <- mod.row$df.null + 1
               } else { 
                 mod.row["n"] <- mod.row$df + mod.row$df.residual
               }
               # Variables indicating estimates for each variable:
               mod.row[paste0(model.tidy$term, ".est")] <-
                 as.list(model.tidy$estimate)
               
               # Variables indicating SE's for each included variable:
               mod.row[paste0(model.tidy$term, ".se")] <-
                 as.list(model.tidy$std.error)
               
               # Variables indicating p.vals for each included variable:
               mod.row[paste0(model.tidy$term, ".pval")] <-
                 as.list(model.tidy$p.value)
               
               # Included variables:
               final.variables <-
                 c(model.tidy$term[!is.na(model.tidy$estimate)][-1])
               
               # Update the final model report:
               final.model <- c(final.model, final.variables)
               
               mod.row$final.variables <-
                 list(final.variables)
               mod.row$final.specification <-
                 list(final.model)
               
               if(inc.tidy.models) {
                 mod.row$tidy.models <- list(model.tidy)
                 mod.row$glance.models <- list(model.glance)
               }
               
               if(inc.full.models) {
                 mod.row$full.models <- list(model.run)
               }

               
               mod.row 
             }) %>% 
      # Bind it up together as a single data.frame
      do.call(what = bind_rows)
    
    
    
    
    
    
    
    # Mark which permutation this data is:
    if(!is.null(perm.index)){
      models.df$permutation.index <- perm.index
    }
    
    models.df
    
  }


# s.curve.update - Calculates the output from a fitted object ---------------

s.curve.update <- function(s.curve.mod, 
                         perm.dat = NULL, # For combining permutation data in (if run externally)
                         perm.pvalues = FALSE # Calculate model-derived p-values
){
  
  if(!is.null(perm.dat)){
    s.curve.mod$perm.test <- perm.dat
    s.curve.mod$permutations <- length(perm.dat)
  }
  # SIGNIFICANCE RATES ----
  
  # Calculate number and percent significant across types:
  s.curve.mod$n.specifications <- nrow(s.curve.mod$results)
  s.curve.mod$n.sig <- sum(s.curve.mod$results$significant)
  s.curve.mod$prop.sig <- mean(s.curve.mod$results$significant)
  
  # Calculate mean estimate:
  s.curve.mod$mean.estimate <- mean(s.curve.mod$results$estimate)
  # Calculate median estimate:
  s.curve.mod$median.estimate <- median(s.curve.mod$results$estimate)
  
  
  # PERMUTATION TEST RATES ----
  
  # Significance rates for permutation test code
  if(!is.null(s.curve.mod$perm.test)){
    
    # Get proportion significance across all permuted models:
    s.curve.mod$perm.prop.sig <-
      vapply(
        s.curve.mod$perm.test,
        function(x){mean(x$significant)},
        1
      )
    
    
    # Mean significance rate across all permutations:
    s.curve.mod$perm.prop.sig.mean <-
      mean(s.curve.mod$perm.prop.sig)
    s.curve.mod$perm.prop.sig.median <-
      median(s.curve.mod$perm.prop.sig)
    
    
    # How many are more significant than the main set:
    s.curve.mod$perm.sig.compare <-
      s.curve.mod$prop.sig >= s.curve.mod$perm.prop.sig
    s.curve.mod$perm.sig.compare.proportion <-
      mean(s.curve.mod$perm.sig.compare)
    
    
    # MEDIAN EFFECT SIZE COMPARISONS ----
    s.curve.mod$perm.median.estimate <-
      vapply(
        s.curve.mod$perm.test,
        function(x){median(x$estimate)},
        1
      )
    
    # Mean significance rate across all models:
    s.curve.mod$perm.median.estimate.mean <-
      mean(s.curve.mod$perm.median.estimate)
    s.curve.mod$perm.median.estimate.median <-
      median(s.curve.mod$perm.median.estimate)
    
    s.curve.mod$perm.median.compare <-
      s.curve.mod$median.estimate >= s.curve.mod$perm.median.estimate
    s.curve.mod$perm.median.compare.proportion <-
      mean(s.curve.mod$perm.median.compare)
    
    # WITHIN SPECIFICATION COMPARISONS (still needs work)----
    
    if(perm.pvalues){
      # Obtain matrix of estimates by specification
      s.curve.mod$estimate.matrix <-
        sapply(
          s.curve.mod$perm.test,
          function(x){x$estimate},
          simplify = "matrix"
        )
      
      perm.quantile.names <-
        c("perm.lower", "perm.median", "perm.upper")
      
      perm.quantiles <-
        c(s.curve.mod$alpha/2, .5, 1-(s.curve.mod$alpha/2))
      
      if(!is.null(s.curve.mod$tail)){
        if(s.curve$tail == "upper"){
          perm.quantile.names <-
            c("perm.median", "perm.upper")
          perm.quantiles <-
            c(.5, 1-s.curve.mod$alpha)
        } else {
          perm.quantile.names <-
            c("perm.lower", "perm.median")
          perm.quantiles <-
            c(s.curve.mod$alpha, .5)
        }
      }
      
      
      
      # Obtain quantiles of estimate from each speficiation:
      s.curve.mod$results[perm.quantile.names] <-
        t(
          apply(
            s.curve.mod$estimate.matrix,
            1,
            quantile,
            probs = perm.quantiles
          ))
      
      
      # Obtain P-value from exact test
      # (comparing absolute values for two-tailed based on CLT,
      # so as to make it direction-agnostic.  Could do better, probably).
      if(is.null(s.curve.mod$tail)){
        
        s.curve.mod$results$perm.p.value <-
          vapply(
            seq_len(nrow(s.curve.mod$results)),
            function(n){
              # What percentage of estimates are at least as extreme
              # as the obtained one?
              mean(
                abs(s.curve.mod$estimate.matrix[n,]) >=
                  abs(s.curve.mod$results$estimate[n])
              )
            }, 1)
        
      } 
      
      if (s.curve.mod$tail == "upper") {
        
        s.curve.mod$results$perm.p.value <-
          vapply(
            seq_len(nrow(s.curve.mod$results)),
            function(n){
              # What percentage of estimates are at least as positive
              # as the obtained one?
              mean(
                s.curve.mod$estimate.matrix[n,] >=
                  s.curve.mod$results$estimate[n]
              )
            }, 1)
        
      } 
      
      if (s.curve.mod$tail == "lower") {
        
        s.curve.mod$results$perm.p.value <-
          vapply(
            seq_len(nrow(s.curve.mod$results)),
            function(n){
              # What percentage of estimates are at least as negative/low
              # as the obtained one?
              mean(
                s.curve.mod$estimate.matrix[n,] <=
                  s.curve.mod$results$estimate[n]
              )
            }, 1)
        
      }
      
      # Critical value test, boolean:
      s.curve.mod$results$perm.significant <-
        s.curve.mod$results$perm.p.value <= s.curve.mod$alpha
      
    }
  }  # End permutation test code
  
  # FINAL OUTPUT: ----
  # extra diagnostic information:
  
  s.curve.mod$ordering <-
    data.frame(
      estimate = order(s.curve.mod$results$estimate),
      p.value = order(s.curve.mod$results$p.value),
      AIC = order(s.curve.mod$results$AIC),
      BIC = order(s.curve.mod$results$BIC)
    )
  
  if(!is.null(s.curve.mod$perm.test) && perm.pvalues) {
    s.curve.mod$ordering$perm.p.value <-
      order(s.curve.mod$results$perm.p.value)
  }
  
  # print summary to screen for easy reading:
  s.curve.mod$report <-
    paste0(
      "Percentage of significant p-values in treatment term", "\n",
      "across all models is ",
      round(100*s.curve.mod$prop.sig, 2), "% (", 
      s.curve.mod$n.sig, " of ", s.curve.mod$n.specifications, " specifications)\n")  
  
  cat(s.curve.mod$report)
  
  
  # Print permutation summary to screen for easy reading:
  if(!is.null(s.curve.mod$perm.test)){
    s.curve.mod$perm.report.significance  <-   paste0(
      "\n",
      "Across ", s.curve.mod$permutations, " permutations, ",
      "the main p-curve analysis had the same or more significant\n",
      "results than ",
      round(100*s.curve.mod$perm.sig.compare.proportion, 2), "% ",
      "of permuted p-curve sets.\n",
      "Mean significance rate: ",
      round(100*s.curve.mod$perm.prop.sig.mean, 2), "%\n",
      "Median significance rate:",
      round(100*s.curve.mod$perm.prop.sig.median, 2), "%\n"
    )
    
    s.curve.mod$perm.report.median  <-   paste0(
      "\n",
      "Across ", s.curve.mod$permutations, " permutations, ",
      "the main p-curve analysis had an equivalent or larger median \n",
      "effect size than ",
      round(100*s.curve.mod$perm.median.compare.proportion, 2), "% ",
      "of permuted p-curve sets.\n",
      "Mean of median estimate: ",
      round(s.curve.mod$perm.median.estimate.mean, 3), "\n",
      "Median of median estimates: ",
      round(s.curve.mod$perm.median.estimate.median, 3), "\n"
    )
    
    cat(s.curve.mod$perm.report.significance)
    cat(s.curve.mod$perm.report.median)
    
  }
  
  s.curve.mod
  
}

# Subsetting tool ---------------------------------------------------------


s.curve.subset <- function(s.curve.mod, ...){
  # Filters a subset of the data based on criteria
  s.curve.mod$results <-
    eval(substitute(
      s.curve.mod$results %>% 
        filter(...)))
  
  s.curve.mod$spec.list <- 
    s.curve.mod$spec.list %>% 
    filter(specification.index %in% 
             s.curve.mod$results$specification.no)
  
  
  if(!is.null(s.curve.mod$perm.test)){
    s.curve.mod$perm.test <-
        s.curve.mod$perm.test %>% 
          map(~.x %>% 
                filter(specification.index %in% 
                         s.curve.mod$results$specification.no)
          )
  }
  s.curve.update(s.curve.mod)
}



# Plotting tool for s.curve output ----------------------------------------



s.curve.plot <-
  function(
    s.curve.mod,
    what = "estimate",
    title = NULL,
    plot.order = NULL,
    decreasing = TRUE,
    show.sig = TRUE,
    include.permutations = TRUE,
    pointsize = 2,
    x.alt = NULL,
    plot.theme = NULL
  ){
    
    order.name <- setNames(c("Estimated Unstandardized Effect Size", 
                             "P-value", "Permutation Test Calculated P-value", "AIC", "BIC"),
                           c("estimate", "p.value", "perm.p.value", "AIC", "BIC"))
    
    # Create a new plotting environment
    env <- new.env(parent = globalenv())
    
    env$what <- what
    env$order.name <- order.name
    env$title <- title
    env$plot.order <- plot.order
    env$decreasing <- decreasing
    env$show.sig <- show.sig
    env$include.permutations <- include.permutations
    env$tail <- s.curve.mod$tail
    env$pointsize <- pointsize
    env$plot.theme <- plot.theme
    
    # Sort by the provided metric:
    if(!is.null(plot.order)){
      new.order <- order(s.curve.mod$results[plot.order], decreasing = decreasing)
      
      env$plot.dat <-  s.curve.mod$results[new.order,]
      env$x.label <- paste0("Specification, Sorted by ", order.name[plot.order])
    } else {
      env$plot.dat <- s.curve.mod$results
      env$x.label <- "Specification Number"
    }
    
    if(is.null(x.alt) | all(is.na(x.alt))){
    env$plot.dat$xval <- seq_len(nrow(env$plot.dat))
    } else { 
    env$plot.dat$xval <- x.alt
    }
    
    env$y.label <- order.name[what]
    
    env$sig.label <- 
      paste0("Significant\nat p < ", substring(s.curve.mod$alpha, 2))
    
    if(s.curve.mod$tail %in% c("upper", "lower")){
      env$sig.label <- 
        paste0(env$sig.label, ",\n", "one-tailed")
    }
    
    if (!is.null(s.curve.mod$permutations) & what == "estimate" & include.permutations){
      env$CI.bound <- ifelse(is.null(s.curve.mod$tail), 
                             paste0(1 - s.curve.mod$alpha, "% CI"),
                             paste0(1 - 2*s.curve.mod$alpha, "% CI")
      )
    }
    
    if (!is.null(plot.theme)){
      env$theme_choice <- 
        match.fun(paste0("theme_", plot.theme))
    } else {
      env$theme_choice <- theme_minimal
    }
    
    # plot
    newplot <-
      with(env, {
        
        plot.dat$yval <- plot.dat[[what]]
        
        newplot <- 
          ggplot(plot.dat,
                 aes(x=xval,
                     y=yval)) + 
          theme_choice() + 
          ylab(y.label) +
          xlab(x.label) +
          ggtitle(title)
        
        if(show.sig){
          newplot <- newplot + 
            geom_point(aes(color=significant), size = pointsize) +
            scale_color_manual(
              values = c(`FALSE` = "black", `TRUE` = "red"),
              name = sig.label)
        } else {
          newplot <- newplot + geom_point(size = pointsize)
        }
        
        if ("perm.median" %in% names(plot.dat) && 
            what == "estimate" && 
            include.permutations){
          
          newplot <-
            newplot +
            geom_line(aes(y=perm.median, linetype = "Median")) +
            scale_linetype_manual(
              name = "Permutation\nTest",
              values = c("Median" = "solid", CI.bound = "dotted"))
            
            if(!is.na(tail) & tail == "upper"){  
              newplot <-
                newplot + 
                geom_line(aes(y=perm.upper, linetype = CI.bound))
            }    
          
          if(!is.na(tail) & tail == "lower"){  
            newplot <-
              newplot + 
              geom_line(aes(y=perm.lower, linetype = CI.bound))
            
          }
        }
        newplot
      })
    newplot 
  }




# Density Plots -----------------------------------------------------------



s.curve.sig.density.plot <- 
  function(
    s.curve.mod, 
    subtitle.add = NULL
  ){
    env <- new.env(parent = globalenv())
    
    env$perm.prop.sig <- s.curve.mod$perm.prop.sig
    env$prop.sig <- s.curve.mod$prop.sig
    env$subtitle.add <- subtitle.add
    
      with(env, {
        ggplot(mapping = aes(x = perm.prop.sig)) + 
          stat_density(geom = "line") +
          stat_density(geom = "area", fill = "blue", alpha = .5) + 
          geom_vline(xintercept = prop.sig, color = "red", linetype = 4) + 
          theme_minimal() + 
          xlab("Proportion of Significant Models") + 
          ggtitle(label = "Density of Simulated-Null Significance Rates (Permutation Test)",
                  subtitle = paste0(subtitle.add, " (Actual Data Significance Rate ", 
                                    round(100*prop.sig, 1), "%)")
          )})
  }

s.curve.est.density.plot <- 
  function(
    s.curve.mod, 
    subtitle.add = NULL
    ){
    env <- new.env(parent = globalenv())
    
    env$median.estimate <- s.curve.mod$median.estimate
    env$perm.median.estimate <- s.curve.mod$perm.median.estimate
    env$subtitle.add <- subtitle.add
  
    with(env, {
      ggplot(mapping = aes(x = perm.median.estimate)) + 
        stat_density(geom = "line") +
        stat_density(geom = "area", fill = "green", alpha = .4) + 
        geom_vline(xintercept = median.estimate, color = "red", linetype = 4) + 
        theme_minimal() + 
        xlab("Median Effect Size") + 
        ggtitle(label = "Density of Simulated-Null Median Effect Sizes (Permutation Test)",
                subtitle = paste0(subtitle.add, " (Actual Data Median Effect Size ", round(median.estimate, 3), ")") 
        )
      })
  }




# Tablemaking tool for s.curve output -------------------------------------



s.curve.table <- function(s.curve.mod, ordering = NULL){
  
  table.displaylist <-
      intersect(
        c("formulas", "subset.used", "weights.used", 
          "estimate", "std.error", "p.value",
          "perm.lower", "perm.median", "perm.upper"),
        names(s.curve.mod$results)
        )

  
  order.touse <- 
    if(!is.null(ordering)){
      order(s.curve.mod$results[[ordering]], decreasing = TRUE)
    } else { TRUE }
  
  s.table <- 
    data.frame(model = 1:nrow(s.curve.mod$results))
  
  s.table[table.displaylist] <-
    s.curve.mod$results[order.touse, table.displaylist]

  s.table
}
