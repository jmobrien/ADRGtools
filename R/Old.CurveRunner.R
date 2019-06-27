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
          unlist(unlist(
            lapply(names(subsettings), function(sub){
              lapply(names(weightings), function(weightvar){
                lapply(formulas, function(form){
                  
                  FUN <- match.fun(mod.type)

                  params <- 
                    list(
                      formula = form
                    )
                  
                  if (length(w.frame) > 1 || !is.na(w.frame)){
                    f.data$weightstouse <- w.frame
                    params$weights <- f.data[sub,][["weightstouse"]]
                  }
                  
                    
                  params$data <- f.data[sub,]
                  
                  model.run <- 
                    do.call(FUN, params)
                  
                  
                  # Clustering the model:
                  if(cluster) {
                    
                    # Load relevant packages:
                    require(sandwich)
                    require(lmtest)
                    
                    model.run.cluster <-
                        cl(dat = f.data, fm = model.run, cluster = f.data[[cluster.var]])
                    
                    model.tidy <-
                      tidy(model.run.cluster)
                    
                  } else if(!is.null(robust.se)) {
                    
                    # Load relevant packages:
                    require(sandwich)
                    require(lmtest)
                    
                    # Do standard error adjustment as called for by robust.se:
                    model.run.robust <- 
                       coeftest(model.run, 
                                 vcov = vcovHC(mod.toadjust, type=robust.se)
                        )
                    
                    model.tidy <-
                      tidy(model.run.robust)
                    
                  } else {  # If neither of these just calculates from the main models: 
                    
                    # Gets tidied output (data frame) for concatenation:
                    model.tidy <-
                      lapply(model.runs, tidy)
                    
                  }
                  
                  
                })
              })
            })
      , recursive = FALSE)
      , recursive = FALSE)
        
        
        
        
        
        
        
        # Set names:
        names(model.runs) <- full.names
        names(models.tidy) <- full.names
        
        # Prepares for duplicated runs where subsetting or weighting was done:
        multiple <- length(subsettings) * length(weightings)
        
        # Makes model output:
        models.df <- data.frame(formulas = rep(formula.names, multiple),
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
        # (pulls out estimates for the specific covariate in the
        # covariate class as named:)
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
        # models.df$AIC <- sapply(model.runs, AIC)
        
        
        # Boolean indicating which models contain which variables:
        # (individual predictor/outcomes, not clusters)
        models.df[vars.list] <-
          lapply(vars.list, function(v){
            # Checks against existince in full element grid
            # (created earlier outside of function)
            apply(mod.grid.all, 1, FUN=function(z){
              v %in% z
            })})
        
        if (!is.null(subsets) & !is.null(weights)){
          
          weight.subset.index <- 
            expand.grid(
              weights.names, 
              subset.names,
              stringsAsFactors = FALSE
            )
          
          models.df[subset.names] <-
            lapply(subset.names, function(x){
              # repeats true/falses based on whether it matches the index above
              do.call(rep,
                      list(
                      c(x == weight.subset.index[[2]]),
                        each = length(formulas)
                      ))
            })
          
          models.df[weights.names] <-
            lapply(weights.names, function(x){
              # repeats true/falses based on whether it matches the index above
              do.call(rep,
                      list(
                        c(x == weight.subset.index[[1]]),
                        each = length(formulas)
                      ))
            })
          
        }
        
        if (!is.null(subsets) & is.null(weights)){
          
          models.df[names(subsettings)] <-
            lapply(names(subsettings), function(x){
              # repeats true/falses based on whether it matches the index above
              rep(x == names(subsettings), each = length(formulas))
            })
          
        }
        
        if (is.null(subsets) & !is.null(weights)){
          
          models.df[names(weightings)] <-
            lapply(names(weightings), function(x){
              # repeats true/falses based on whether it matches the index above
              rep(x == names(weightings), each = length(formulas))
            })
          
        }
        
        
        
        
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