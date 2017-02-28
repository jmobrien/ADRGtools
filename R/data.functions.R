read.qualtrics <-
  function(
    file,
    stringsAsFactors = FALSE,
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


dupInfo <-
  function(x, useNA="ifany"){
  dupdat <- data.frame(table(x, useNA = useNA))
  dupdat <- dupdat[dupdat$Freq > 1,]
  dupdat$position <-
    lapply(dupdat$x, function(y){which(x %in% y)})
  dupdat
    }
