
nonFinishCheck <- function(dat){
  require(tidyverse)
  num.vars <- length(dat) 
  
  # Missingness - for each case, how many variables were not answered that were answered by over 65% of people?
  nonmiss.index <- 
    dat %>% 
    map_lgl(~mean(is.na(.x)) < .65) # are fewer than 65% of the people missing this variable?
  
  
  row.missingness <- # Obtain a list of row-wise missingness:
    apply(dat, 1, 
          FUN = function(ind.row){
            
              ind.row %>% # Take a single row
              as.character %>% #convert to character
              is.na # identify what cells are NA
          }) %>% 
    as_tibble
            
  miss.check <- 
    #What's the total number missing where we could have expected presence?  
    row.missingness %>% 
    map_int(~sum(.x & nonmiss.index))
    
  end.check <- 
  # (used below) use run-length encoding to determine what the 
  # length of missingness at the end (ifany) is 
    row.missingness %>% 
    map(rle)
  
  
  end.check.df <-           
    map_dfr(end.check,
            ~{
              # Did they finish? (i.e., is the last run NOT made up of NA's)?
              finishes <- FALSE == .x$values[length(.x$values)] # element value at last place 
              # Binary numeric indicating if they quit (invert finishes)
              quits <- as.numeric(!finishes)
              # Numeric: How long before the end did they quit? (uses "quits" to set to 0 if finished)
              missing.end <- quits*.x$lengths[length(.x$lengths)]
              # What was the last entry?
              last.entry <- num.vars - missing.end # subtracts the # of NA's at end from total # vars
              # What percent of the data was done?
              percent.present <- last.entry/num.vars # subtracts the # of NA's at end from total # vars
              # Output a dataframe for binding:
              tibble(pos.lastvar.completed = last.entry, 
                     num.vars.left = missing.end,
                     percent.completed = percent.present, 
                     finished = finishes
                     )
            })
  
  bind_cols(
    tibble(num.skipped = miss.check),
    end.check.df
  )
}



