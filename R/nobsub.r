nobsub <- function(id, data) {
  if (missing(data)) 
    data <- NULL
  no.data <- is.null(data)
  if (no.data) {
    data <- sys.frame(sys.parent())
  }
  else {
    if (!is.data.frame(data)) 
      data <- data.frame(data)
  }
  mf <- match.call()
  mf.id <- mf[[match("id", names(mf))]]
  id <- eval(mf.id, data, enclos = sys.frame(sys.parent()))
  if (is.null(id)) 
    stop("Argument 'id' must be specified.")
  if (any(is.na(id))) 
    stop("Argument 'id' should not contain any NAs.")
  
  out <- data.frame(tapply(id, id, length))
  names(out) <- "nobs"
  
  return(out)
}
