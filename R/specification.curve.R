
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
    dat, # datafile
    
    mod.type = lm, #takes lm, glm (NOT YET IMPLEMENTED)
    mod.family = NULL, # if family needed (NOT YET IMPLEMENTED)
    alpha = .05, # plausible alpha value (if one tailed, will test against p <= .1 on that side)
    tail = NULL, # One tailed test?  Takes "upper" and "lower"
    subsets = NULL, # runs subsets of data based on the specifications listed here (vector of conditions)
    subsets.exclude = TRUE, # if subsets added, includes an un-subsetted version
    weights = NULL, # vector of weight variables names to add to runs, re-named if desired
    weights.exclude = TRUE, # Includes an unweighted version where weights added 
    permutations = NULL, # number of permutations for p-curve
    cluster = FALSE, # Cluster robust standard errors
    cluster.var = NULL, # Variable on which to cluster
    robust.se = NULL, # robustness adjustment-provide parameters in capital letters e.g. "HC1"
    cat.percent = TRUE, # displays summary output of % significant at the end of the data for convenience in interactive mode. Set to false if using as a part of an Rmarkdown file.
    keep.tidy.models = TRUE, # Set to false for large model samples
    keep.full.models = FALSE, # Set to false for large model samples
    model.only = FALSE # Outputs the model for later use, rather than running
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
        mod.type = mod.type,
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
    
    # Converts formula names to character vector:
    s.curve.mod$formula.names <- 
      sapply(s.curve.mod$formulas, deparse, width.cutoff = 500)    
    
    # Length of formula list:
    s.curve.mod$n.formulas <- 
      length(s.curve.mod$formulas)
    
    # Initialize names of everything: 
    full.names <- formula.names

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
    
    s.curve.mod$spec.list <- 
      expand.grid(
        formulas,
        names(weightings),
        names(subsettings),
        stringsAsFactors = FALSE
      )
      
    
    

        
  
    
    # RUN MAIN SPECIFICATION CURVE ----
    output[c("results", "tidy.models", "raw.models")] <-
      curveRunner(f.data=dat, full.models = TRUE)
    
    
    
    # RUN PERMUTATION TEST ----
    
    if(is.numeric(permutations)){
      
      # Save a nested list of individual data frames
      # like the one we just made above:
      perm.dat <- dat
      set.seed(2018)
      output$perm.test <-
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
            curveRunner(f.data = perm.dat,
                        full.models = FALSE)
          },
          simplify=FALSE)
    } #
    
    # SIGNIFICANCE RATES ----
    
    # Calculate number and percent significant across types:
    output$n.sig <- sum(output$results$significant)
    output$prop.sig <- mean(output$results$significant)
    
    # Significance rates for permutation test code
    if(is.numeric(permutations)){
      
      # Get proportion significance across all permuted models:
      output$perm.prop.sig <-
        sapply(
          output$perm.test,
          function(x){mean(x$significant)}
        )
      
      # Mean significance rate across all models:
      output$perm.prop.sig.mean <-
        mean(output$perm.prop.sig)
      
      # How many are more significant than the main set:
      output$perm.sig.compare <-
        output$prop.sig >= output$perm.prop.sig
      output$perm.sig.compare.mean <-
        mean(output$perm.sig.compare)
      
      # WITHIN SPECIFICATION COMPARISONS ----
      
      # Obtain matrix of estimates by specification
      estimate.matrix <-
        sapply(
          output$perm.test,
          function(x){x$estimate},
          simplify = "matrix"
        )
      
      # Obtain quantiles of estimate from each speficiation:
      output$results[c("perm.lower", "perm.median", "perm.upper")] <-
        t(
          apply(
            estimate.matrix,
            1,
            quantile,
            probs = c(.025, .50, .975)
          ))
      
      
      # Obtain P-value from exact test
      # (comparing absolute values for two-tailed based on CLT,
      # so as to make it direction-agnostic.  Could do better, probably).
      output$results$perm.p.value <-
        sapply(
          1:n.formulas,
          function(n){
            # What percentage of estimates are at least as extreme
            # as the obtained one?
            mean(abs(estimate.matrix[n,]) >=
                   abs(output$results$estimate[n]))
          })
      
      # Critical value test, boolean:
      output$results$perm.significant <-
        output$results$perm.p.value < critical.value
      
    }  # End permutation test code
    
    # FINAL OUTPUT: ----
    # extra diagnostic information:
    output$formulas <- formula.names
    output$n.formulas <- n.formulas
    output$permutations <- permutations
    output$critical.value <- critical.value
    output$ordering <-
      data.frame(
        estimate = order(output$results$estimate),
        p.value = order(output$results$p.value)
      )
    if(is.numeric(permutations)) {
      output$ordering$perm.p.value <-
        order(output$results$perm.p.value)
    }
    
    # print summary to screen for easy reading:
    if(cat.percent){
      cat(
        paste0(
          "Percentage of significant p-values in treatment term", "\n",
          "across all models is ",
          round(100*output$prop.sig, 2), "%"))
    }
    
    # Print permutation summary to screen for easy reading:
    if(cat.percent & is.numeric(permutations)){
      cat(
        paste0(
          "\n\n",
          "Across ", permutations, " permutations, ",
          "the main p-curve analysis had more significant\n",
          "results than ",
          round(100*output$perm.sig.compare.mean, 2), "% ",
          "of permuted p-curve sets.\n",
          "(mean significance rate: ",
          round(100*output$perm.prop.sig.mean, 2), "%)",
          "\n"
        ))
    }
    
    output
    
  }



# curveRunner - processes s-curve objects ---------------------------------

curveRunner <-
  function(
    s.curve.model,
    inc.tidy.models = NULL,
    inc.full.models = NULL,
    perm.index = NULL
  ){

    dat <- s.curve.model$data
    spec.list <- s.curve.model$spec.list
    subsettings <- s.curve.model$subsettings
    weightings <- s.curve.model$weightings
    
    if(is.null(inc.tidy.models)){
      inc.tidy.models <- 
        s.curve.model$keep.tidy.models
    }
    
    if(is.null(inc.full.models)){
      inc.tidy.models <- 
        s.curve.model$keep.full.models
    }
    
    models.df <-
      lapply(seq_len(nrow(spec.list)), 
             # indexing function
             function(ind){
               
               # Initialize output:
               mod.row <- 
                 data.frame(
                   formulas = spec.list[ind, "formula"],
                   stringsAsFactors = FALSE
                 ) 
               
               # Initialize model parameters:
               params <- 
                 list(
                   formula = spec.list[ind, "formula"]
                 )
               
               
               weight.name <- spec.list[ind, "weights"]
               subset.name <- spec.list[ind, "subset"]
               
               # Subsetting setup:
               if (!is.na(subset.name)){
                 # Establish the subset:
                 sub <- subsettings[[subset.name]]
                 # note it in the output:
                 mod.row$subset.used <- subset.name
               } else {
                 # Just keep everything with this:
                 sub <- TRUE
               }
               
               # Weighting setup:
               if (!is.na(weights.name)){
                 # Add the weight variable
                 params$weights <- 
                   weightings[sub, weights.name]
                 # note it in the output:
                 mod.row$weights.used <- weights.name
               }
               
               # Add in data:
               params$data <- dat[sub,]
               
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
                                          type=curve.model$robust.se)
                   )
                 
                 model.tidy <-
                   tidy(model.run.robust)
                 
                 
               } else {  
                 # If neither of these, just calculate from the main model: 
                 
                 # Gets tidied output (data frame) for concatenation:
                 model.tidy <-
                   tidy(model.runs)
                 
                 
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
               mod.row[c("r.squared", "adj.r.squared", "df", "df.residual")] <-
                model.glance[c("r.squared", "adj.r.squared", "df", "df.residual")]
               
               # Model N 
               mod.row["n"] <- mod.row$df + mod.row$df.residual
               
               # Variables indicating estimates for each variable:
               mod.row[paste0(model.tidy$term[-1], ".est")] <- 
                 as.list(model.tidy$estimate[-1])

               # Variables indicating SE's for each included variable:
               mod.row[paste0(model.tidy$term[-1], ".se")] <- 
                 as.list(model.tidy$std.error[-1])

               # Variables indicating p.vals for each included variable:
               mod.row[paste0(model.tidy$term[-1], ".pval")] <- 
                 as.list(model.tidy$p.value[-1])
               
               if(inc.tidy.models) {
                 mod.row$tidy.models <- list(model.tidy)
               }
               
               if(inc.full.models) {
                 mod.row$full.models <- list(model.run)
               }
               
             }) %>% 
      # Bind it up together as a single data.frame
      bind_rows

    # Mark which permutation this data is:
    if(!is.null(perm.index)){
      models.df$permutation.index <- perm.index
    }

models.df



  }



# Plotting tool for s.curve output ----------------------------------------



s.curve.plot <-
  function(
    data,
    title = NULL,
    plot.order = NULL,
    decreasing = TRUE
  ){
    
    # Sort by the provided metric:
    if(!is.null(plot.order)){
      new.order <- order(data$results[plot.order], decreasing = decreasing)
      plot.dat <-  data$results[new.order,]
    } else {
      plot.dat <- data$results
    }
    
    # plot
    newplot <-
      ggplot(plot.dat,
             aes(x=seq_along(estimate),
                 y=estimate)) +
      geom_point(aes(color=significant)) +
      scale_color_manual(
        values = c("black", "red"),
        name = paste0("Significant\nat p = ", data$critical.value)
      ) +
      theme_bw() +
      ylab(plot.order) +
      xlab(paste0("Specification # (sorted by ", plot.order, ")")) +
      ggtitle(title)
    if (!is.null(data$permutations)){
      newplot <-
        newplot +
        geom_line(aes(y=perm.lower, linetype = "95 % CI")) +
        geom_line(aes(y=perm.upper, linetype = "95 % CI")) +
        geom_line(aes(y=perm.median, linetype = "Median")) +
        scale_linetype_manual(
          name = "Permutation\nTest",
          values = c("Median" = "solid", "95 % CI" = "dotted")
        )}
    newplot
  }




# Tablemaking tool for s.curve output -------------------------------------



s.curve.table <- function(scurve, ordering = NULL){
  
  table.displaylist <-
    if(is.null(scurve$permutations)){
      c("formulas", "estimate", "std.error", "p.value")
    } else {
      c("formulas", "estimate", "std.error", "p.value",
        "perm.lower", "perm.median", "perm.upper")
    }
  s.table <- data.frame(model = 1:length(scurve$tidy.models))
  s.table[table.displaylist] <-
    scurve$results[
      # order(scurve$results[[ordering]], decreasing = TRUE),
      table.displaylist
      ]
  
  s.table
}
