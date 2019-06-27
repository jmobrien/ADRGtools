corstars <- function(x, show = "lower"){ 
  require(Hmisc) 
  require(DescTools)
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .10, "+", ""))))
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- Format(R, digits = 3, leading = "drop", sci = NA)
  ## build a new matrix that includes the correlations with their appropriate stars 
  Rnew <- matrix(paste0(mystars, R), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  if(show == "lower"){
    ## remove upper triangle
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew) 
    ## remove last column and return the matrix (which is now a data frame)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    return(Rnew)
  } else if(show == "upper") {
    ## remove upper triangle
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew) 
    ## remove last column and return the matrix (which is now a data frame)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    return(Rnew)
  } else if(show == "all") {
  as.data.frame(Rnew)
  }
}
