

# SRETextract - tool for pulling out JS SRET data  ------------------------

SRETextract <- function(dat,
                        alt.names = NULL,
                        suffix = NULL
){
  # This function should be written back to the full data that you have, like:
  # mydata <- SRETextract(mydata)
  # It will add several variables that relate to number and percent of responses
  # of positive and negative words, along with some checks of # of words seen for
  # diagnostics.
  
  require(stringr)
  
  if(is.null(alt.names)){
    alt.names <- 
      c(SRET.words = "SRET.words", 
        SRET.keys  = "SRET.keys", 
        SRET.time  = "SRET.time")
  } 
  
  if(!all(c("SRET.words", "SRET.keys", "SRET.time") %in% 
          names(alt.names))){
    stop("names are wrong")
  }
  
  
# Setup: wordlists --------------------------------------------------------
  
  wordlist <- 
    c("Funny", "Happy", "Terrible", "Angry", "Ashamed", "Free", 
      "Sorry", "Hateful", "Kind", "Stupid", "Naughty", "Helpful", 
      "Joyful", "Worried", "Playful", "Horrible", "Bad", "Fantastic", 
      "Depressed", "Pleased", "Nasty", "Foolish", "Good", "Brilliant", 
      "Excited", "Nice", "Annoyed", "Content", "Sad", "Upset", 
      "Scared", "Awesome", "Unloved", "Fun", "Unhappy", "Alone", 
      "Cool", "Excellent", "Loved", "Confident", "Unwanted", "Friendly", 
      "Great", "Lost", "Guilty", "Proud", "Lonely", "Mad", 
      "Wicked", "Best", "Glad", "Wonderful")
  
  wordlist.positive <- 
    c("Happy", "Fun", "Brilliant", "Cool", "Good", "Glad", 
      "Loved", "Friendly", "Wonderful",  "Pleased", "Helpful","Proud", 
      "Fantastic", "Confident", "Content", "Joyful",  "Best", "Excited", 
      "Free", "Funny", "Kind", "Playful",   "Great", "Excellent", 
      "Awesome", "Nice")
  
  wordlist.negative <-
    c("Terrible", "Angry", "Ashamed", "Sorry", "Hateful", "Stupid", 
      "Naughty", "Worried", "Horrible", "Bad", "Depressed", "Nasty", 
      "Foolish", "Annoyed", "Sad", "Upset", "Scared", "Unloved", 
      "Unhappy", "Alone", "Unwanted", "Lost", "Guilty", "Lonely", 
      "Mad", "Wicked")
  
  # make reference data frame:
  wordlist.ispositive <- ifelse(wordlist %in% wordlist.positive, 1, 0)
  
  wordlist.reference <- 
    data.frame(
      wordlist,
      is.positive = wordlist.ispositive,
      stringsAsFactors = FALSE
    )  

 # Initial extraction of JSON data ----------------------------------
  
  # Extract the words from each person's responses:
  sret.words <- 
    str_extract_all(dat[[alt.names["SRET.words"]]], 
                    "[[:alpha:]]+")
  
  # Keypresses
  sret.keys <- 
    str_extract_all(dat[[alt.names["SRET.keys"]]], 
                    "[[:alpha:]]+")
  
  # Endorsements:
  sret.agree <- 
    lapply(sret.keys, function(key){ifelse(key=="P", 1, 0)})
  
  # Response times:
  sret.time <- 
    lapply(str_extract_all(
      dat[[alt.names["SRET.time"]]], 
      "[0-9]+\\.?[0-9]*"), 
      as.double)
  
  
  
  # Checking the length:
  length.check <-
    data.frame(
      words.length = sapply(sret.words, length),
      keys.length = sapply(sret.keys, length),
      agree.length = sapply(sret.agree, length),
      time.length = sapply(sret.time, length)
    )
  
  #noting any discrepancies:
  length.check$problemflag <-
    vapply(seq_along(nrow(length.check)), function(i){
      ifelse(
        any(is.na(unlist(length.check[i,])) | 
              all(unlist(length.check[i,]) == 52) | 
              all(unlist(length.check[i,]) == 0)
        ), 0L, 1L)
    }, FUN.VALUE = integer(1))
  
  
  # Create a data frame for every person with their responses:
  sret.data <- 
    mapply(
      data.frame,
      words = sret.words,
      keys = sret.keys,
      agree = sret.agree,
      time = sret.time,
      MoreArgs = list(
        stringsAsFactors = FALSE
      ),
      SIMPLIFY = FALSE
    )
  
  # Adding in a word-position variable, problem flag for RT, valence marker
  sret.data <- 
    lapply(sret.data, function(x){
      
      # Word position (order seen)
      len <- seq_len(nrow(x))
      x$word.position <- len
      
      # Word valence:
      # Compare to list of words:
      ispos.index <- 
        match(x$words, wordlist.reference$wordlist)
      # Note if positive or not
      x$ispositive <- 
        wordlist.reference$is.positive[ispos.index]
      
      # Check for RTs that are < 200
      x$RTsub200 <- ifelse(x$time < 200, 1, 0)
      
      # Check for RT's that are above 3 AMD > median
      xmedian <- median(x$time[x$RTsub200 == 0]) 
      xMAD <- mad(x$time[x$RTsub200 == 0])
      x$RToutlier <- 
        ifelse(
          x$RTsub200 == 0 &
          x$time > (xmedian + 3*xMAD)
          , 1, 0)
      
      x
    })
  

    
  
  # sret.data[[165]]
  # sret.data[[10]]
  
  # Appears to be random 
  # mean(sret.data[[21]]$words %in%
  # sret.data[[249]]$words)
  
  
  
  # Summaries back into the main dataset ------------------------------------
  
output.vars <- 
    c("sret.endorsepos", "sret.endorseneg",
      "sret.RTendorsepos", "sret.RTendorseneg",
      "sret.RTrejectpos", "sret.RTrejectneg",
      "sret.RToverall",
      "sret.totalpos", "sret.totalneg",
      "sret.sub200pos", "sret.sub200neg",
      "sret.outlierpos", "sret.outlierneg",
      "sret.problemflag", "sret.problemdescrip"
    )
                 
  
  # Proportion of positive words endorsed
  dat$sret.endorsepos <-
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
             mean(x$agree[x$ispositive & !x$RTsub200])
      )})
  
  # Proportion of negative words endorsed
  dat$sret.endorseneg <-
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
             mean(x$agree[!x$ispositive & !x$RTsub200])
      )})
  
  # RT of positive words endorsed
  dat$sret.RTendorsepos <-
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
             round(
               mean(x$time[x$ispositive & x$agree & !x$RTsub200 & !x$RToutlier])
               , 0)
      )})
  
  # RT  of negative words endorsed:
  dat$sret.RTendorseneg <-
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
             round(
               mean(x$time[!x$ispositive & x$agree & !x$RTsub200 & !x$RToutlier])
               , 0)
      )})

  # RT of positive words rejected:
  dat$sret.RTrejectpos <-
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
             round(
               mean(x$time[x$ispositive & !x$agree & !x$RTsub200 & !x$RToutlier])
               , 0)
      )})

  # RT of negative words rejected:
  dat$sret.RTrejectneg <-
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
             round(
               mean(x$time[!x$ispositive & !x$agree & !x$RTsub200 & !x$RToutlier])
               , 0)
      )})
  
  # RT of negative words rejected:
  dat$sret.RToverall <-
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
             round(
               mean(x$time[!x$RTsub200 & !x$RToutlier])
               ,0)
      )})
  
  
  # Number of words seen that were positive:
  dat$sret.totalpos <- 
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
      sum(x$ispositive, na.rm=T)
      )})
  
  # Number of words seen that were negative
  dat$sret.totalneg <- 
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
             sum(!x$ispositive, na.rm=T)
    )})

  # Number of positive <200ms words
  dat$sret.sub200pos <- 
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
      sum(x$ispositive & x$RTsub200, na.rm=T)
    )})
  
  # Number of negative <200ms words
  dat$sret.sub200neg <- 
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
      sum(!x$ispositive & x$RTsub200, na.rm=T)
    )})

  # Number of positive outlier words
  dat$sret.outlierpos <- 
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
      sum(x$ispositive & x$RToutlier, na.rm=T)
    )})
  
  # Number of negative outlier words
  dat$sret.outlierneg <- 
    sapply(sret.data, function(x){
      ifelse(nrow(x) == 0, NA, 
      sum(!x$ispositive & x$RToutlier, na.rm=T)
    )})
  
  #Problem variables: 
  dat[c("sret.problemflag", "sret.problemdescrip")] <-
    do.call(rbind,
      lapply(sret.data, function(x){
        
        # NA's for missing data
        if(nrow(x)==0) {
          flag <- NA
          descrip <- NA
        } else {
          
          # Set flags to 0 assuming no problems
          flag <- 0
          descrip <- ""
          sep <- NULL
          
          # Flag for >10% too-fast responding:
          if(mean(x$RTsub200) >= .1){
            flag <- 1
            descrip <- "10%sub200"
            sep <- "; "
          }
          # Flag for >15% problematic responding:
          if(mean(rowSums(x[c("RTsub200", "RToutlier")])) >= .15){
            flag <- 1
            descrip <- paste0(descrip, sep, "15%badRT")
            sep <- "; "
          }
          # Flag for uniform responding:
          if(mean(x$agree[!x$RTsub200]) %in% 0:1){
            flag <- 1
            descrip <- paste0(descrip, sep, "uniform")
            sep <- "; "
          }
          # Flag for potentially noncompliant responding:
          if(any(table(x$agree[!x$RTsub200]) <= 5)){
            flag <- 1
            descrip <- paste0(descrip, sep, "near-uniform")
            sep <- "; "
          }
        }
        # Output the data for subsequent row-binding:
        data.frame(flag, descrip)
      }))

  # Error checking:
  if(any(length.check$problemflag == 1)){
    errors <- which(length.check$problemflag)
    dat$sret.problemflag[errors] <- 1
    dat$sret.problemdescrip[errors] <-
      ifelse(dat$problemdescrip[errors] == "",
             "error", 
             paste0(dat$problemdescrip[errors], "; ", "error")
      )
  }
     
  
# Adds the suffix in as necessary:
  
if(!is.null(suffix)){
  
  names(dat)[names(dat) %in% output.vars] <- paste0(output.vars, suffix)
  
}

  
  dat
  
}

