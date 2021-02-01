split.esm <- function(data, id, include = NULL, exclude = NULL) {
  if (is.null(data)) 
    stop("Argument 'data' must be specified.")
  
  if (!is.data.frame(data)) {
    data <- data.frame(data)
  }
  
  mf <- match.call()
  mf.id <- mf[[match("id", names(mf))]]
  id <- eval(mf.id, data, enclos = sys.frame(sys.parent()))
  if (is.null(id)) 
    stop("Argument 'id' must be specified.")
  if (any(is.na(id))) 
    stop("Argument 'id' should not contain any NAs.")
  
  varnames <- names(data)
  nvars    <- length(varnames)
  
  if (!is.null(include) & !is.null(exclude)) {
    exclude <- NULL
    if (is.numeric(include)) {
      include <- varnames[include]
    }
    warning("Both arguments 'include' and 'exclude' were specified. ",
            "The following variables were included in the individual datasets: ",
            paste0(include, c(rep(", ", length(include) - 1), "."), collapse = ""))
  }
  
  if (is.null(include) & is.null(exclude)) {
    vars <- 1:length(varnames)
  }
  
  if (!is.null(include)) {
    if (!(is.character(include) | is.numeric(include))) 
      stop("Argument 'include' must either be a character or a numeric vector.")
    if (is.character(include)) {
      vars.pos <- lapply(include, function(x) {
        pos <- charmatch(x, varnames)
        if (is.na(pos)) 
          stop("Variable '", x, "' not found in the data frame.", 
               call. = FALSE)
        if (pos == 0L) 
          stop("Multiple matches for variable '", x, 
               "' in the data frame.", call. = FALSE)
        return(pos)
      })
      vars <- unique(unlist(vars.pos))
    } else {
      vars.pos <- unique(round(include))
      if (min(vars.pos) < 1 | max(vars.pos) > nvars) { 
        stop("Variables positions must be between 1 and ", 
             nvars, ".")
      }
      vars <- vars.pos
    }
  }
  
  if (!is.null(exclude)) {
    if (!(is.character(exclude) | is.numeric(exclude))) 
      stop("Argument 'exclude' must either be a character or a numeric vector.")
    if (is.character(exclude)) {
      vars.pos <- lapply(exclude, function(x) {
        pos <- charmatch(x, varnames)
        if (is.na(pos)) 
          stop("Variable '", x, "' not found in the data frame.", 
               call. = FALSE)
        if (pos == 0L) 
          stop("Multiple matches for variable '", x, 
               "' in the data frame.", call. = FALSE)
        return(pos)
      })
      vars <- (1:length(varnames))[-unique(unlist(vars.pos))]
    } else {
      vars.pos <- unique(round(exclude))
      if (min(vars.pos) < 1 | max(vars.pos) > nvars) { 
        stop("Variables positions must be between 1 and ", 
             nvars, ".")
      }
      vars <- (1:length(varnames))[-vars.pos]
    }
  }
  
  temp <- list()
  ids <- sort(unique(id))
  
  for (i in 1:length(ids)) {
    temp[[i]] <- data[id == ids[i], vars]
  }
  rm(i)
  
  if (!is.character(ids)) {
    ids <- as.character(ids)
  }
  
  names(temp) <- ids
  
  return(temp)
}
