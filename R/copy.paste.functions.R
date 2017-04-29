#These all write to the clipboard, so that you can just code as you go by calling them and then command-V to paste.  See below for examples.

#Outputs as indvidual lines
snip <- function(input) {
  pb <- pipe("pbcopy", "w")
  write(print(input), file=pb)
  close(pb)
}


#Quoted snip: copies and pastes in comma separated list, with quotes as needed
qsnip <- 
  function(input) {
    options(useFancyQuotes = FALSE)
    pb <- pipe("pbcopy", "w")
    output <- if(is.character(input)) dQuote(input) else input
    cat(print(output), file=pb, sep = ", ")
    close(pb)
  }

#Combined Snip: puts a c() around the output, so you can just drop it directly into a lot of things
csnip <- 
  function(input) {
    options(useFancyQuotes = FALSE)
    pb <- pipe("pbcopy", "w")
    output <- if(is.character(input)) {
      paste0("c(", paste(dQuote(input), collapse =  ", "), ")")
    } else {
      paste0("c(", paste(input, collapse =  ", "), ")") 
    }
    cat(print(output), file=pb)
    close(pb)
  }

fsnip <- 
  function(input) {
    options(useFancyQuotes = FALSE)
    pb <- pipe("pbcopy", "w")
    output <- if(is.character(input)) {
      paste(input, collapse =  " + ")
    }
    cat(print(output), file=pb)
    close(pb)
  }

# Simpler, works somewhat differently.
csnip2 <-
  function(input){
    pb <- pipe("pbcopy", "w")
    dput(input, file=pb,
         control = c())
    close(pb)
  }

# snip(names(iris))
# #This outputs:
# Sepal.Length
# Sepal.Width
# Petal.Length
# Petal.Width
# Species
# 
# qsnip(names(iris))
# #This outputs:
# "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"
# 
# csnip(names(iris))
# #This outputs:
# c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")