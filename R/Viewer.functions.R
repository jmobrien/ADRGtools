viewstargazer <- function(..., keep.stat="n"){
  # Writes a model (or, more usefully, a list of models) 
  # to the viewer window of R so you can examine it more easily.
  require(stargazer)
  params <- list(...)
  viewer <- getOption("viewer")
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  params <-
    c(params,
      list(type="html"
           , dep.var.caption = ""
           , model.numbers = FALSE
           , header = FALSE
           , keep.stat = keep.stat
           , out=htmlFile
           , star.cutoffs = c(.05, .01, .001)))

capture.output(
  do.call(stargazer, params),
  file = "/dev/null"
)

  viewer(htmlFile)
}



viewhtmlreg <- function(...){
  require(texreg)
  params <- list(...)
  viewer <- getOption("viewer")
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  params <-
    c(params,
      list(file=htmlFile)
    )

capture.output(
  do.call(htmlreg, params),
  file = "/dev/null"
)

  viewer(htmlFile)
}