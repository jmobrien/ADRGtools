#These all write to the clipboard, so that you can just code as you go by calling them and then command-V to paste.  See below for examples.

snip <- function(input) {
#Standard snip: Outputs as indvidual lines
  pb <- pipe("pbcopy", "w")
  write(print(input), file=pb)
  close(pb)
}


esnip <- function(input){
  # Excel snip: copies dataframes to the clipboard in form pasteable into excel
  clip <- pipe("pbcopy", "w")
  write.table(input, file = clip, sep = "\t", row.names = FALSE)
  close(clip)
}

qsnip <- 
#Quoted snip: copies and pastes in comma separated list, with quotes as needed
  function(input) {
    options(useFancyQuotes = FALSE)
    pb <- pipe("pbcopy", "w")
    output <- if(is.character(input)) dQuote(input) else input
    cat(print(output), file=pb, sep = ", ")
    close(pb)
  }

csnip <- 
#Combined Snip: puts a c() around the output in quotes, so you can just drop it directly into a lot of things
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
# Formula snip - copies unquoted with + in between terms
  function(input) {
    options(useFancyQuotes = FALSE)
    pb <- pipe("pbcopy", "w")
    output <- if(is.character(input)) {
      paste(input, collapse =  " + ")
    }
    cat(print(output), file=pb)
    close(pb)
  }

csnip2 <-
# Simpler, works somewhat differently.
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