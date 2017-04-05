
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

    mod.type = lm, #takes lm, glm 
    grouping = NULL,
    r.effects = "intercepts",
    mod.family = NULL, # if family needed for glm
    critical.value = .05,
    cat.percent = TRUE, # displays summary output of % significant at the end of the data for convenience in interactive mode. Set to false if using as a part of an Rmarkdown file.
    permutations = NULL # number of permutations for p-curve

    ) {

    # Library calls ----
    require(broom)
    require(ggplot2)
    if(mod.type == lmer){require(lmerTest)}

    # Initialize output list ----
    output <- list()

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
    if( any(formulas.rhs == "") ) {
      warning(
        "NOTE: set includes one or more null models."
      )}
    
    if( mod.type == lmer ) {
      paste0(formulas.rhs, "(1 | ", grouping, ")")
      }
      


    # Adds left-hand side (outcomes), make formulas:
    formulas <-
      lapply(
        as.vector(outer(outcomes, formulas.rhs,
                        paste, sep=" ~ ")),
        as.formula)

    # Converts formula names to character vector:
    formula.names <- sapply(formulas, deparse, width.cutoff = 500)    # Length of specification list:
    n.formulas <- length(formulas)

    # Function for running models ----
    #(used here, and in permutation form below)
    curveRunner <-
      function(
        f.data,
        full.models=FALSE
        ){
        # Runs models using the supplied model type, puts in list
        # (Needs more work to support multiple types of models)
        model.runs <-
          lapply(formulas, function(forms){
            FUN <- match.fun(mod.type)
            FUN(forms, data=f.data, family=mod.family)
          })
        
        if (mod.type == lmer){
          models.tidy <-
            lapply(model.runs, function(mod){
              sumry <- summary(mod)$coefficients
              sumry.names <- rownames(sumry)
              sumry <- as.data.frame(cbind(sumry.names, sumry))
              names(sumry) <- c("term", "estimate", "std.error", "df", "statistic", "p.value")
              sumry
            })
              
        } else {
          
        # Gets tidied output (data frame) for concatenation:
        models.tidy <-
          lapply(model.runs, tidy)
        
        }
        
        # Set names:
        names(model.runs) <- formula.names
        names(models.tidy) <- formula.names

        # Makes model output:
        models.df <- data.frame(formulas = formula.names,
                             stringsAsFactors = FALSE)

        # ADD TREATMENT EFFECT RESULTS TO DATA FRAME:
        # Estimate:
        models.df$estimate <-
          sapply(models.tidy, function(x){
            x[x$term %in% treatment,]$estimate
            })

        # Standard error:
        models.df$std.error <-
          sapply(models.tidy, function(x){
            x[x$term %in% treatment,]$std.error
            })

        # p-value:
        models.df$p.value <-
          sapply(models.tidy,
             function(x){x[x$term %in% treatment,]$p.value})

        # Boolean indicating significance
        models.df$significant <-
          models.df$p.value <= critical.value

        # ADD RESULTS FOR EACH COVARIATE SET:
        # (does each *GROUP* of covariates as a unit, not each
        # possible covariate)
        # Estimate:
        models.df[paste0(names(cov.list), ".est")] <-
          lapply(names(cov.list), function(x){
            sapply(models.tidy, function(y){
              if (any(y$term %in% cov.list[[x]])) {
                y[y$term %in% cov.list[[x]],]$estimate
              } else {
                NA
              }})})

        # Standard error:
        models.df[paste0(names(cov.list), ".se")] <-
          lapply(names(cov.list), function(x){
            sapply(models.tidy, function(y){
              if (any(y$term %in% cov.list[[x]])) {
                y[y$term %in% cov.list[[x]],]$std.error
              } else {
                NA
              }})})

        # p-values
        models.df[paste0(names(cov.list), ".pval")] <-
          lapply(names(cov.list), function(x){
            sapply(models.tidy, function(y){
              if (any(y$term %in% cov.list[[x]])) {
                y[y$term %in% cov.list[[x]],]$p.value
              } else {
                NA
              }})})

        # Boolean for significance
        models.df[paste0(names(cov.list), ".sig")] <-
          lapply(models.df[paste0(names(cov.list), ".pval")],
                 function(x){
                   x <= critical.value
                 })

        # FINAL STUFF--FIT, SEARCHING:
        # AIC for each model:
        models.df$AIC <- sapply(model.runs, AIC)


        # Boolean indicating which models contain which variables:
        # (individual predictor/outcomes, not clusters)
        models.df[vars.list] <-
          lapply(vars.list, function(v){
            # Checks against existince in full element grid
            # (created earlier outside of function)
            apply(mod.grid.all, 1, FUN=function(z){
              v %in% z
            })})

        # OUTPUT:
        # a list containing both the models
        if (full.models == TRUE){
        list(
          models.df,
          models.tidy,
          model.runs
        )
      } else {
          models.df
      }
      }

    # RUN MAIN SPECIFICATION CURVE ----
    output[c("results", "tidy.models", "raw.models")] <-
      curveRunner(f.data=dat, full.models = TRUE)



    # RUN PERMUTATION TEST ----

    if(is.numeric(permutations)){

      # Save a nested list of individual data frames
      # like the one we just made above:
      perm.dat <- dat

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


# Plotting tool -----------------------------------------------------------



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



# Tablemaking function -------------------------------------------------------


s.curve.table <- function(scurve, ordering = "estimate"){

  table.displaylist <-
    if(is.null(scurve$permutations)){
      c("formulas", "estimate", "std.error", "p.value")
    } else {
      c("formulas", "estimate", "std.error", "p.value",
        "perm.lower", "perm.median", "perm.upper")
    }
  s.table <- data.frame(model = 1:scurve$n.formulas)
  s.table[table.displaylist] <-
    scurve$results[
      order(scurve$results[[ordering]], decreasing = TRUE),
      table.displaylist
      ]

  s.table
}
