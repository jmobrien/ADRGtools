

PreregTestSubsamples <- 
  # Takes partial subsamples of data and runs them
  function(data 
           , group = NULL
           , ratio = .5
           , iterations = 100
           , file = NULL
           , sample.unit = NULL
           , ...
  ){
    
    rowref <- 
      data.frame(
        row = 1:nrow(data)
      )
    
    # Give extra variables depending on case:
    if(!is.null(group)){rowref$group <- data[[group]]}
    if(!is.null(sample.unit)){rowref$sample.unit <- data[[sample.unit]]}
    
    # Get rows for the simplest case: wide data, single-group:
    if(is.null(group) & is.null(sample.unit)){
      # Construct row samples
      row.samples <- 
        lapply(seq_len(iterations), 
               function(x){
                 rowref %>% 
                   sample_frac(ratio, replace = FALSE) %>% 
                   pull(row)
               })
      
    } 
    
    # Get rows for grouped wide-form:
    if(!is.null(group) & is.null(sample.unit)){
      # Construct row samples
      row.samples <- 
        lapply(seq_len(iterations), 
               function(x){
                 rowref %>% 
                   group_by(group) %>% 
                   sample_frac(ratio, replace = FALSE) %>% 
                   pull(row)
               })
      
    } 
    
    # Get rows for ungrouped long-form:
    if(is.null(group) & is.null(sample.unit)){
      
      # Construct list of unique sample units:
      uniques <- 
        unique(rowref$sample.unit)
      
      row.samples <- 
        lapply(seq_len(iterations), 
               function(x){
                 # Pull out the ratio of unique IDs:
                 subsample.units <- 
                   sample(uniques, round(length(uniques)*ratio,0), replace = FALSE)
                 # Get the row numbers from the full dataset that are in this sample:
                 rowref %>% 
                   filter(sample.unit %in% subsample.units) %>% 
                   pull(row)
               })
    }
    
    # Grouped long-form case:
    if(!is.null(group) & !is.null(sample.unit)){
      
      # Construct list of unique sample units:
      uniques.list <- 
        with(rowref, tapply(sample.unit, group, unique))
      
      uniques <- 
        do.call(rbind,
                lapply(names(uniques.list), function(name){
                  data.frame(
                    group = rep(name, length(uniques.list[[name]])),
                    sample.unit = uniques.list[[name]],
                    stringsAsFactors = FALSE
                  )}))
      
      row.samples <- 
        lapply(seq_len(iterations), 
               function(x){
                 # Pull out the ratio of unique cases from each group:
                 subsample.units <- 
                   uniques %>% 
                   group_by(group) %>% 
                   sample_frac(ratio, replace = FALSE) %>% 
                   pull(sample.unit)
                 # Get the row numbers from the full dataset that are in this sample:
                 rowref %>% 
                   filter(sample.unit %in% subsample.units) %>% 
                   pull(row)
               })
    }
    
    # Finally ,take the row.samples variable and construct a selection table:
    
    match.table <- 
      setNames(
        as.data.frame(
          lapply(row.samples,
                 function(samp){
                   rowref$row %in% samp
                 })
        ),
        paste0("samp", seq_len(iterations))
      )
    
    if(is.null(group) & is.null(sample.unit)){
      return(
        list(
          data = data %>% select(...),
          sample.table = match.table
    )
      )}
    
    if(!is.null(group) & is.null(sample.unit)){
      return(
        list(
          data = data %>% select(..., group),
          sample.table = match.table
    )
      )}
    
    if(is.null(group) & !is.null(sample.unit)){
      return(
        list(
          data = data %>% select(..., sample.unit),
          sample.table = match.table
    )
      )}
    
    if(!is.null(group) & !is.null(sample.unit)){
      return(
        list(
          data = data %>% select(..., group, sample.unit),
          sample.table = match.table
    )
    )}
  } 



PreregCheckout <- function(repository
                           , number = NULL)
  {
  
  samples <- length(repository$sample.table)
  
  if(is.null(number)) {
    number <- sample(1:samples, 1)
  }
  
  selections <- repository$sample.table[[number]]
  
  dat.subsample <- repository$data[selections,]
  
  dat.subsample$subsample.number <- number
  dat.subsample$checkout.date <- date()
    
  message(sprintf("Sample checked out is %i of %i", number, samples))
  dat.subsample
}