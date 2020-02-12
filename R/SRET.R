# TODO: add in switch for raw summaries.
# TODO: 

# SRETextract - tool for pulling out JS SRET data  ------------------------

SRETextract <- function(dat,
                        alt.names = NULL,
                        suffix = NULL,
                        raw.out.path = NULL,
                        raw.shape = "wide",
                        raw.dropmissing = TRUE # if FALSE, keep the extra rows for super-easy merging.  TRUE drops missing cases and does not save those ID's in main data
){
  # This function should be written back to the full data that you have, like:
  # mydata <- SRETextract(mydata)
  # It will add several variables that relate to number and percent of responses
  # of positive and negative words, along with some checks of # of words seen for
  # diagnostics.
  
  require(stringr)

  # If variable names not given, uses standard names:  
  if(is.null(alt.names)){
    alt.names <- 
      c(SRET.words = "SRET.words", 
        SRET.keys  = "SRET.keys", 
        SRET.time  = "SRET.time")
  } 
  
  # throws error if all the given/default names aren't present:
  if(!all(c("SRET.words", "SRET.keys", "SRET.time") %in% 
          names(alt.names))){
    stop("names are wrong")
  }
  
  
  # Setup: wordlists --------------------------------------------------------
  
  # Complete word-list
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
  
  # Positive words
  wordlist.positive <- 
    c("Happy", "Fun", "Brilliant", "Cool", "Good", "Glad", 
      "Loved", "Friendly", "Wonderful",  "Pleased", "Helpful","Proud", 
      "Fantastic", "Confident", "Content", "Joyful",  "Best", "Excited", 
      "Free", "Funny", "Kind", "Playful",   "Great", "Excellent", 
      "Awesome", "Nice")
  
  # Negative words
  wordlist.negative <-
    c("Terrible", "Angry", "Ashamed", "Sorry", "Hateful", "Stupid", 
      "Naughty", "Worried", "Horrible", "Bad", "Depressed", "Nasty", 
      "Foolish", "Annoyed", "Sad", "Upset", "Scared", "Unloved", 
      "Unhappy", "Alone", "Unwanted", "Lost", "Guilty", "Lonely", 
      "Mad", "Wicked")
  
  # Make reference data frame for positive/negative words:
  wordlist.ispositive <- ifelse(wordlist %in% wordlist.positive, 1, 0)
  
  # Data frame:
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
    vapply(seq_len(nrow(length.check)), function(i){
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
    c("endorsepos", "endorseneg",
      "RTendorsepos", "RTendorseneg",
      "RTrejectpos", "RTrejectneg",
      "RToverall",
      "totalpos", "totalneg",
      "sub200pos", "sub200neg",
      "outlierpos", "outlierneg",
      "problemflag", "problemdescrip"
    )
  
  sret.output.vars <- paste0("sret.", output.vars)
  
  dat[sret.output.vars] <-
    do.call(rbind.data.frame, 
            lapply(sret.data, function(x){
              if(nrow(x) == 0 | all(is.na(x$words))){
                
                # Fill everything in with NA's if data is missing 
                # (empty string or one NA) 
                rep(NA, length(output.vars))
                
              } else {
                
                # Proportion of positive words endorsed
                endorsepos <- mean(x$agree[x$ispositive & !x$RTsub200])
                # Proportion of negative words endorsed
                endorseneg <- mean(x$agree[!x$ispositive & !x$RTsub200])
                # RT of positive words endorsed
                RTendorsepos <-
                  round(digits = 0,
                        mean(x$time[x$ispositive & x$agree & 
                                      !x$RTsub200 & !x$RToutlier]))
                # RT  of negative words endorsed:
                RTendorseneg <-
                  round(digits = 0,
                        mean(x$time[!x$ispositive & x$agree & 
                                      !x$RTsub200 & !x$RToutlier]))
                # RT of positive words rejected:
                RTrejectpos <-
                  round(digits = 0,
                        mean(x$time[x$ispositive & !x$agree & 
                                      !x$RTsub200 & !x$RToutlier]))
                # RT of negative words rejected:
                RTrejectneg <- 
                  round(digits = 0,
                        mean(x$time[!x$ispositive & !x$agree & 
                                      !x$RTsub200 & !x$RToutlier]))
                # RT of negative words rejected:
                RToverall <- 
                  round(digits = 0,
                        mean(x$time[!x$RTsub200 & !x$RToutlier]))
                # Number of words seen that were positive:
                totalpos <- sum(x$ispositive, na.rm=T)
                # Number of words seen that were negative
                totalneg <- sum(!x$ispositive, na.rm=T)
                # Number of positive <200ms words
                sub200pos <- sum(x$ispositive & x$RTsub200, na.rm=T)
                # Number of negative <200ms words
                sub200neg <-  sum(!x$ispositive & x$RTsub200, na.rm=T)
                # Number of positive outlier words
                outlierpos <- sum(x$ispositive & x$RToutlier, na.rm=T)
                # Number of negative outlier words
                outlierneg <- sum(!x$ispositive & x$RToutlier, na.rm=T)
                
                #Problem variables: 
                # Set flags to 0 assuming no problems
                problemflag <- 0
                problemdescrip <- ""
                sep <- NULL
                
                # problemflag for >10% too-fast responding:
                if(mean(x$RTsub200) >= .1){
                  problemflag <- 1
                  problemdescrip <- "10%sub200"
                  sep <- "; "
                }
                
                # problemflag for >15% problematic responding:
                if(mean(rowSums(x[c("RTsub200", "RToutlier")])) >= .15){
                  problemflag <- 1
                  problemdescrip <- paste0(problemdescrip, sep, "15%badRT")
                  sep <- "; "
                }
                
                # problemflag for uniform responding:
                if(mean(x$agree[!x$RTsub200]) %in% 0:1){
                  problemflag <- 1
                  problemdescrip <- paste0(problemdescrip, sep, "uniform")
                  sep <- "; "
                }
                
                # problemflag for potentially noncompliant responding:
                if(any(table(x$agree[!x$RTsub200]) <= 5)){
                  problemflag <- 1
                  problemdescrip <- paste0(problemdescrip, sep, "near-uniform")
                  sep <- "; "
                }
                
                # Output the new data.frame:
                data.frame(endorsepos, endorseneg,
                           RTendorsepos, RTendorseneg,
                           RTrejectpos, RTrejectneg,
                           RToverall,
                           totalpos, totalneg,
                           sub200pos, sub200neg,
                           outlierpos, outlierneg,
                           problemflag, problemdescrip, 
                           stringsAsFactors = FALSE
                )
                
              } # end variable creation
            } # End anon function
            ) # End lapply
    ) # End do.call
  
  # Error checking:
  if(any(length.check$problemflag == 1)){
    errors <- which(length.check$problemflag == 1)
    dat$sret.problemflag[errors] <- 1
    dat$sret.problemdescrip[errors] <-
      ifelse(dat$sret.problemdescrip[errors] == "",
             "error", 
             paste0(dat$sret.problemdescrip[errors], "; ", "error")
      )
  }
  
  
  # Raw output --------------------------------------------------------------
  
  if(!is.null(raw.out.path)){
    
    # Adds the ID variable:
    output.vars <- c("rawmatchID", output.vars)
    
    sret.output.vars <- c("sret.rawmatchID", sret.output.vars)
    
    dat$sret.rawmatchID <- 
      paste0(
        round(as.numeric(Sys.time()), 0), # Start with the current system time 
        # add a random number from the lowest place value of the available numbers up to the next place value (so if 550, from 100:999)
        sample(
          x = 10^floor(log10(nrow(dat))):(10^(floor(log10(nrow(dat)))+1)-1), 
          size = nrow(dat),
          replace = FALSE)
             )
    
    
    
    raw.vars <-
      construct.all(c(sort(wordlist.positive), sort(wordlist.negative)), ".", 
                    c("agree", "time.ms", "order", "RTsub200", "RToutlier"))
    
    rawdat <- 
      do.call(rbind.data.frame, 
              lapply(sret.data, function(x){
                if(nrow(x) == 0 | all(is.na(x$words))){
                  
                  # Fill everything in with NA's if data is missing 
                  # (empty string or one NA) 
                  rep(NA, length(raw.vars))
                  
                } else {
                  
                  # Sorts each case first by valence, then alphabetically:
                  x.sort <- 
                    x[order(1-x$ispositive, x$words), 
                      c("words", "ispositive", "agree", "time", "word.position", 
                        "RTsub200", "RToutlier")]
                  
                  setNames(
                    # Make a run of the words in question:
                    as.list(
                      round(digits = 0,
                            do.call(c, x.sort[-1])))
                    ,
                    # Give them the right names, dropping any words if they don't 
                    # have them
                    construct.all(x.sort$words, ".",
                                  c("positive", "agree", "time.ms", "order",
                                    "RTsub200", "RToutlier"))
                  ) # End SetNames
                  
                } # End else
              }) # End lapply
      ) #end do.call (and creation)
    
    rawdat <- 
      setNames(
        cbind(dat[sret.output.vars], rawdat),
        c(output.vars, names(rawdat))
      )
    
    # Sort to match the original data:
    rawdat <- rawdat[match(rawdat$rawmatchID, dat$sret.rawmatchID),]

    
    if(raw.shape %in% c("long", "LONG")){
      rawdat$index <- seq_len(nrow(rawdat))
      rawdat <-
        reshape(rawdat,
                direction = "long",
                timevar = "word",
                idvar = "rawmatchID",
                varying = list(
                  paste0(wordlist, ".order"),
                  paste0(wordlist, ".positive"),
                  paste0(wordlist, ".agree"),
                  paste0(wordlist, ".time.ms"),
                  paste0(wordlist, ".RTsub200"),
                  paste0(wordlist, ".RToutlier")
                ), 
                v.names = c(
                  "order",
                  "positive",
                  "agree", 
                  "time.ms", 
                  "RTsub200", 
                  "RToutlier")
                , times = wordlist
        )
      
      rawdat$problemflag <- ifelse(is.na(rawdat$RTendorsepos) & is.na(rawdat$RTendorseneg), NA, rawdat$problemflag)
      rawdat$word <- ifelse(is.na(rawdat$RTendorsepos) & is.na(rawdat$RTendorseneg), NA, rawdat$word)
      rawdat <- rawdat[order(rawdat$index, rawdat$order),]
      rawdat$index <- NULL
      
    }
    
    # Drop the extra rawmatchID's unless requested to keep them
    if(raw.dropmissing){
      # Drop the ID's from missing cases:
      dat$sret.rawmatchID[is.na(dat[alt.names["SRET.keys"]])] <- NA
      # Filter the raw data to contain only the remaining ID's
      rawdat <- rawdat[rawdat$rawmatchID %in% dat$sret.rawmatchID,]
    }
    
    # Outputting to console:
    if(raw.out.path %in% c("HERE", "here")){
      message("outputting raw data ONLY (with summaries)")
      return(rawdat)
    } else {
      # Outputting to file:
      message(paste0("raw data written to ", raw.out.path, 
                     ", outputting summaries to original data frame"))
      write.csv(rawdat, raw.out.path, row.names = FALSE)
    } # End output
  } # End raw data section
  
  # Final output ------------------------------------------------------------
  
  # Adds the variable suffix in if requested:
  if(!is.null(suffix)){
    # Replace the output names with the suffix-appended ones:
    names(dat)[names(dat) %in% sret.output.vars] <- paste0(sret.output.vars, suffix)
    
  }  
  
  # Output the original data frame plus the new variables:
  dat
  
}





