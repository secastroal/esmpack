calc.mcent.ti <- function(x, id, data, expand = TRUE) {
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
  mf.x <- mf[[match("x", names(mf))]]
  mf.id <- mf[[match("id", names(mf))]]
  x <- eval(mf.x, data, enclos = sys.frame(sys.parent()))
  id <- eval(mf.id, data, enclos = sys.frame(sys.parent()))
  if (is.null(x)) 
    stop("Argument 'x' must be specified.")
  if (is.null(id)) 
    stop("Argument 'id' must be specified.")
  if (any(is.na(id))) 
    stop("Argument 'id' should not contain any NAs.")
  
  var <- get.timeinvar(x, id, na.rm = FALSE)
  
  varm <- mean(var, na.rm = TRUE)
  
  var <- var - varm
  
  if (expand) {
    id.order     <- unique(id)
    observations <- nobsub(id)
    
    observations.match <- observations[match(id.order, row.names(observations)), ]
    var.match          <- var[match(id.order, names(var))]
    
    res <- var.match[rep(1:length(var.match), times = observations.match)]
  }
  else {
    res <- var
  }
  return(res)
}