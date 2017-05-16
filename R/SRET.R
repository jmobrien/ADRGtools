
SRETextract <- function(dat){
# This function should be written back to the full data that you have, like:
# mydata <- SRETextract(mydata)
  

require(stringr)

# Extract the words from each person's responses:
sret.words <- str_extract_all(dat$SRET.words, "[[:alpha:]]+")
sret.keys <- str_extract_all(dat$SRET.keys, "[[:alpha:]]+")
sret.agree <- lapply(sret.keys, function(key){ifelse(key=="P", 1, 0)})
sret.time <- lapply(str_extract_all(dat$SRET.time, "[0-9]+\\.?[0-9]*"), as.double)


# Create a data.frame for every person with their responses:
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

# Adding in a word-position variable (maybe useful later) 
sret.data <- 
  lapply(sret.data, function(x){
      len <- seq_len(nrow(x))
      x$word.position <- 
        ifelse(len == 1, NA, len)
      x
    })
   
# Checking the length:
length.check <-
  data.frame(
    words.length = sapply(sret.words, length),
    keys.length = sapply(sret.keys, length),
    agree.length = sapply(sret.agree, length),
    time.length = sapply(sret.time, length)
  )

# sret.data[[165]]
# sret.data[[10]]

# Appears to be random 
# mean(sret.data[[21]]$words %in%
# sret.data[[249]]$words)


# Create wordlist for reference -------------------------------------------

wordlist <- c("Funny", "Happy", "Terrible", "Angry", "Ashamed", "Free", "Sorry", "Hateful", "Kind", "Stupid", 
  "Naughty", "Helpful", "Joyful", "Worried", "Playful", "Horrible", "Bad", "Fantastic", "Depressed", 
  "Pleased", "Nasty", "Foolish", "Good", "Brilliant", "Excited", "Nice", "Annoyed", "Content", "Sad", 
  "Upset", "Scared", "Awesome", "Unloved", "Fun", "Unhappy", "Alone", "Cool", "Excellent", "Loved", 
  "Confident", "Unwanted", "Friendly", "Great", "Lost", "Guilty", "Proud", "Lonely", "Mad", "Wicked", 
  "Best", "Glad", "Wonderful")

wordlist.positive <- 
  c("Happy", "Fun", "Brilliant", "Cool", "Good", "Glad", "Loved", "Friendly", "Wonderful",  
  "Pleased", "Helpful","Proud", "Fantastic", "Confident", "Content", "Joyful",  "Best", 
  "Excited", "Free", "Funny", "Kind", "Playful",   "Great", "Excellent", "Awesome", "Nice")

wordlist.negative <-
  c("Terrible", "Angry", "Ashamed", "Sorry", "Hateful", "Stupid", "Naughty", "Worried", "Horrible", 
    "Bad", "Depressed", "Nasty", "Foolish", "Annoyed", "Sad", "Upset", "Scared", "Unloved", "Unhappy", 
    "Alone", "Unwanted", "Lost", "Guilty", "Lonely", "Mad", "Wicked")

# make reference data frame:
wordlist.ispositive <- ifelse(wordlist %in% wordlist.positive, 1, 0)

wordlist.reference <- 
  data.frame(
    wordlist,
    is.positive = wordlist.ispositive,
    stringsAsFactors = FALSE
  )


# Add valence to each set of results --------------------------------------


# Add the valence to each one:
sret.data <- 
  lapply(sret.data,
         function(x){
           #Match to main worlist
           ispos.index <- match(x$words, wordlist.reference$wordlist)
           #Write to individual data frame:
           x$ispositive <- 
             wordlist.reference$is.positive[ispos.index]
           x
         })




# Summaries back into the main dataset ------------------------------------



# Count of words seen
dat$sret.num.words.seen <- 
  sapply(sret.data, function(x){
    len <- nrow(x)
    ifelse(len == 1, 0, len)})
    
# Number of words seen that were positive:
dat$sret.pos.words.seen <-
  sapply(sret.data, function(x){
    sum(x$ispositive, na.rm=T)
  })

# Number of words seen that were negative
dat$sret.neg.words.seen <-
  dat$sret.num.words.seen -
  dat$sret.pos.words.seen

# Percent of words seen that were positive
dat$sret.percent.pos.words <-
  dat$sret.pos.words.seen /
  dat$sret.num.words.seen

# percent of words seen that were negative
dat$sret.percent.neg.words <-
  1 - dat$sret.percent.pos.words


# Number of positive words agreed-to:
dat$sret.num.pos.agree <-
  sapply(sret.data, function(x){
    ifelse(nrow(x) == 0, NA, 
    sum(x$ispositive & x$agree)
    )})

# Number of negative words agreed-to:
dat$sret.num.neg.agree <- 
  sapply(sret.data, function(x){
    ifelse(nrow(x) == 0, NA, 
    sum((!x$ispositive) & x$agree)
    )})

# Percentage of positive words agreed-to:
dat$sret.percent.pos.agree <- 
  dat$sret.num.pos.agree /
  dat$sret.pos.words.seen

# Percentage of negative words agreed-to:
dat$sret.percent.neg.agree <- 
  dat$sret.num.neg.agree /
  dat$sret.neg.words.seen

dat

}

