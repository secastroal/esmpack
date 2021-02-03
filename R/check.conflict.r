check.conflict <- function(x, y, data, threshold, out = 1) {
  # check if 'data' argument has been specified
  
  if (missing(data))
    data <- NULL
  
  no.data <- is.null(data)
  
  if (no.data) {
    data <- sys.frame(sys.parent())
  } else {
    if (!is.data.frame(data))
      data <- data.frame(data)
  }
  
  mf <- match.call()
  
  # get 'x' and 'y' arguments (will be NULL when unspecified)
  
  mf.x <- mf[[match("x",  names(mf))]]
  mf.y <- mf[[match("y", names(mf))]]
  x    <- eval(mf.x, data, enclos=sys.frame(sys.parent()))
  y    <- eval(mf.y, data, enclos=sys.frame(sys.parent()))
  
  # check that 'x' and 'y' have been specified
  
  if (is.null(x))
    stop("Argument 'x' must be specified.")
  if (is.null(y))
    stop("Argument 'y' must be specified.")
  
  # check 'out' argument
  
  if (is.character(out)) {
    out <- pmatch(out, c("logical", "ind", "data"))
    if (is.na(out))
      stop("Argument 'out' must be either set to 'logical', 'ind', or 'data'.")
  }
  
  if (!(out %in% 1:3))
    stop("Argument 'out' must be set to 1, 2, 3.")
  
  if (!threshold) {
    stop("Argument 'threshold' must be specified.")
  }
  
  if (!is.numeric(threshold)) {
    stop("Argument 'threshold' must be a scalar.")
  }
  
  if (length(threshold) > 1) {
    warning("Argument 'threshold' has length > 1 and only the first element will be used.")
    threshold <- threshold[1]
  }
  
  diff <- abs(x - y)
  
  conflict <- (diff <= threshold)
  
  if (out == 1) {
    return(conflict)
  }
  
  if (out == 2) {
    return(which(conflict))
  }
  
  if (out == 3) {
    if (no.data) {
      return(data.frame(x, y)[conflict,, drop = FALSE])
    } else {
      return(data[conflict,, drop = FALSE])
    }
  }
}
