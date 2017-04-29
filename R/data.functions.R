# This reads the native output of qualtrics without having to pre-clean the extra lines at the top
# And it takes the data from those removed lines (info about the prompt text), and puts them into an attribute.
read.qualtrics <-
  function(
    file,
    stringsAsFactors = FALSE
    ){
    header.dat <- read.csv(file, stringsAsFactors = stringsAsFactors)
    main.dat <- read.csv(file, skip=3, header = FALSE, stringsAsFactors = stringsAsFactors)
    names(main.dat) <- names(header.dat)
    content <- as.character(header.dat[1,])
    for(i in 1:length(main.dat)) {
      attr(main.dat[[i]], "prompt") <- content[i]
    }
    main.dat
  }



# Function that can handle reverse coded items when making composites
makevar <- 
  function(
    data, # data for use
    vars, # Non-reverse coded variables (vector in quotes)
    rev.vars = NULL, #Reverse coded variables (vector in quotes)
    rev.max = NULL, # Maximum value of scale (used for reverse coding)
    rev.min = 1 #minimum amount of scale (used for reverse coding, assumed 1)
  ) {
    # Function that can handle reverse coded items when making composites
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


merge.with.order <- function(x,y, ..., sort = T, keep_order){
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  # From https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
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
