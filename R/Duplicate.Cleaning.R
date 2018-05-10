DuplicatePattern <- 
  function(
    data, # The data in question 
    id,    # The id variable
    marker.suffix = NULL
  ){
    # Create datafile:
    workdat <- eval(substitute(data), envir = parent.frame())
    # Saves ID variable:
    idvar <- workdat[[id]]
    
    # Make sure that there are duplicates:
    if(!any(duplicated(idvar))){
      return("no duplicates")
    }
    
    # Identifies all ID's that are duplicates in dataset:
    dupes <- unique(idvar[duplicated(idvar)])
    
    # Get the stitching together info:
    lapply(dupes, function(ind.id){
      
      # for each duplicate, identify the relevant rows in the data
      dupes.rows <- 
        which(idvar == ind.id)
      
      # Extract the patterns of missing data for each case:
      misspattern <- 
        apply(workdat[dupes.rows,], 1, 
              FUN = function(indrow){
                # use run-length encoding to determine what the 
                # patterns of missing data are:
                rle(is.na(as.character(indrow)))
              })
      
      
      # position of last entry before the string of NA's
      case.summary <- 
        cbind(
          data.frame(
            id = ind.id,
            rows = dupes.rows
          ),       
          do.call(rbind,
                  lapply(misspattern, function(case){
                    # Did they finish?
                    finishes <- case$values[length(case$values)]==FALSE
                    # Quit (inversion)
                    quits <- as.numeric(!finishes)
                    # How long before the end did they quit? (set to 0 if finished)
                    missing.end <- quits*case$lengths[length(case$lengths)]
                    # What was the last entry?
                    last.entry <- length(workdat) - missing.end
                    # Output a dataframe for binding:
                    data.frame(last.entry, finishes, stringsAsFactors = FALSE)
                  })))
      
      # Figure out how to bind them together
      for(i in seq_len(nrow(case.summary))) {
        # Start with 1, and end at the last entry
        if (i == 1) {
          start <- 1
          end <- case.summary$last.entry[i]
          # Then, if the last or previous entries is a finish, mark the subsequent entities NA:
        } else if (any(case.summary$finishes[1:(i-1)])) {
          start[i] <- NA
          end[i] <- NA
          # Then on the subsequent ones, if we haven't reached a finish yet 
          # BUT the end is less than the maximimum ending point thus far (so nothing new)
          # then NA:
        } else if (case.summary$last.entry[i] <= max(end, na.rm = TRUE)) {
          start[i] <- NA
          end[i] <- NA
        } else {
          # Otherwise, start at the next value past the previous end
          # and end at the last non NA value:
          start[i] <- max(end, na.rm = TRUE) + 1
          end[i] <- case.summary$last.entry[i]
        }
      }
      
      
      case.summary$start <- start
      case.summary$end <- end
      
      # Finally, if we're on the get to end with no finish, then finish with last case
      case.summary$end[max(which(!is.na(case.summary$end)))] <- length(workdat)
      
      case.summary
    })
    
    
    
    
    
  }

DuplicateCombine <- 
  function(data, id){
    
    # Temporary dataset
    tempdat <- eval(substitute(data))
    
    # Get ID name to pass to the pattern tool:
    id.name <- deparse(substitute(id))
    
    # Run the pattern tool:
    cleaningpattern <- DuplicatePattern(tempdat, id.name)
    
    if(cleaningpattern[1] == "no duplicates"){
      message("No duplicates found; returning data frame as-is.")
      assign("duplicate.report", "No duplicates found", envir = .GlobalEnv)
      return(data)
    }
    
    # Pull out the relevant cases:
    combineddat <- 
      do.call(rbind,
              lapply(cleaningpattern, function(ind.id){
                
                ind.id <- ind.id[which(!is.na(ind.id$start)),]
                
                # 
                do.call(
                  cbind,
                  lapply(seq_len(nrow(ind.id)), function(i){
                    tempdat[ind.id$rows[i], ind.id$start[i]:ind.id$end[i]]
                  }))
              })
      )
    
    # Identify the rows to move in the original dataset:
    cut.rows <- 
      do.call(c,
              lapply(cleaningpattern, function(x){x$rows})
      )
    
    # Mark which rows are 
    duplicate.cleaned.name <- paste0("duplicate.cleaned", marker.suffix)
    tempdat[duplicate.cleaned.name] <- 0
    combineddat[duplicate.cleaned.name] <- 1
    
    # Combine them into a final dataset
    final.dat <- rbind(tempdat[-cut.rows,], combineddat)
    
    # Write the duplicate report out to the global environment:
    assign("duplicate.report", cleaningpattern, envir = .GlobalEnv)
    
    # Output the corrected dataset:
    final.dat
  }


# debugonce(DuplicatePattern)
# DuplicatePattern(testdat, "ID")
# DuplicatePattern(testdat.nodupes, "ID")
# debugonce(DuplicateCombine)
# DuplicatePattern(testdat, "ID")