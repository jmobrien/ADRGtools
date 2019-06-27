IdeationTest <- 
  function(
    data, # the data to examine bare variable
    id.var, # ID variable, in quotes
    word.list, # Vector of PERL-style regular expressions to match
    check.file = NULL, # filepath for random additional sample for possible word list improvement
    viewer = TRUE # display viewer of random additional sample
  ){
    
    # Set aside the ID variable:
    id <- data[id.var]
    
    # Drop the ID variable from data (don't want to scan it):
    dat <- data[names(data) != id.var]
    
    # Create a data frame with,
    # for each scanned variable: 
    # 1. a hit marker (1/0)
    # 2. The specifically word-list item
    # 3. The full text that includes the match
    hit.dat <- 
      # Column bind the 3 rows produced from each variable:
      # (lapply outputs a list, do.call feeds the list items as arguments to cbind)
      do.call(cbind,
              # Go through each variable and do the following:
              lapply(dat, function(variable){
                
                # 1. Check each one for a hit on every word in the word list
                # Produces data frame w/dimensions [cases x length(word list)]
                # Logical output, TRUE/FALSE hit for each case/word pair:
                hits <- 
                  as.data.frame(
                    sapply(word.list$LIST.ENTRIES, 
                           function(word){
                             # Logical output of match/no match, PERL-style for lookbacks/aheads:
                             grepl(word, variable, perl = TRUE, ignore.case = TRUE)
                           }
                           , simplify = FALSE
                           , USE.NAMES = TRUE
                    ))
                
                # 2. If we find one, then mark it true
                hit.any <- 
                  as.numeric(
                    apply(hits, 1, any)
                  )
                
                # 3. If we have a match on a particular variable, 
                # then paste together and write the matching words from the wordlist.
                # If no match for that var, then ""
                matches <- 
                  apply(hits, 1, function(case){
                    ifelse(
                      any(case), 
                      paste0(names(hits)[case], collapse = ", "),
                      "") 
                  })
                
                # 4. Construct and output a dataframe with the 
                # hit indicator, the wordlist match, and the 
                # variable content
                data.frame(hit.any, 
                           matches, 
                           variable, 
                           stringsAsFactors = FALSE
                )
              }) # End variable lapply
      ) # End do.call cbinding into a data.frame
    
    # Update names in new data frame to show which variable:
    names(hit.dat) <- 
      paste0(
        # 3x each variable name:
        rep(names(dat), each = 3),
        # endings in order:
        c(".hit", ".wordmatch", "")
      )
    
    # Look down each row to examine just the ".hit" variables for each case.
    # Mark if that case has any hits:
    any.hit <- 
      apply(
        # Only every third variable starting with the first (the ".hit" vars):
        hit.dat[seq(1, length(hit.dat), by = 3)],
        # look down rows:
        1, 
        # Tell me if any in a row == 1
        function(x){any(x==1)}
      )
    
    # Add ID variable back to the beginning:
    all.dat <- 
      cbind(id, hit.dat)
    
    # Extra check -------------------------------------------------------------
    
    if(!is.null(check.file)){
      index.check <- 
        sample(
          seq_len(nrow(all.dat))[!any.hit], 
          50
        )
      
      check.dat <- all.dat[index.check,]
      
      write.csv(check.dat, check.file, row.names = FALSE)
      
      if(viewer){View(check.dat)}
      
    }
    
    
    # concerning cases output -------------------------------------------------
    
    # Keep only the cases that have at least one hit, and output:
    all.dat[any.hit,]
  }
