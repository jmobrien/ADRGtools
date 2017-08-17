# TODO:
# cordays/coraverage - different ways to look at longitudinal data automatically.



# read.qualtrics - imports Qualtrics native .csv format -------------------


read.qualtrics <-
  function(
  # This reads the native output of qualtrics without having to pre-clean the
  # extra lines at the top And it takes the data from those removed lines (info
  # about the prompt text), and puts them into an attribute.
    file, # path to the qualtrics file
    stringsAsFactors = FALSE 
    ){
    # Imports the file twice: once to get the header data/variable names, once
    # to get the real data
    header.dat <- read.csv(file, stringsAsFactors = stringsAsFactors)
    main.dat <- read.csv(file, skip=3, header = FALSE, stringsAsFactors = stringsAsFactors)
    names(main.dat) <- names(header.dat)
    content <- as.character(header.dat[1,])
    for(i in 1:length(main.dat)) {
      attr(main.dat[[i]], "prompt") <- content[i]
    }
    main.dat
  }