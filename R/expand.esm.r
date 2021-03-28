expand.esm <- function(data, id, time, tinterval = 1, include = NULL, exclude = NULL) {
  if (is.null(data)) 
    stop("Argument 'data' must be specified.")
  
  if (!is.data.frame(data)) {
    data <- data.frame(data)
  }
  
  varnames <- names(data)
  nvars    <- length(varnames)
  
  if (!is.null(include) & !is.null(exclude)) {
    exclude <- NULL
    if (is.numeric(include)) {
      include <- varnames[include]
    }
    warning("Both arguments 'include' and 'exclude' were specified. ",
            "The following variables were included in the final dataframe: ",
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
  
  # Create reduced dataset given the variables to include or exclude
  data <- data[, vars]
  varnames <- names(data)
  nvars    <- length(varnames)
  
  # New data.frame excluding the variables id and time
  
  if (!(is.character(c(id, time)) | is.numeric(c(id, time)))) 
    stop("Argument 'id' and 'time' must either be a character or a numeric vector.")
  if (is.character(c(id, time))) {
    idvar.pos <- lapply(c(id, time), function(x) {
      pos <- charmatch(x, varnames)
      if (is.na(pos)) 
        stop("Variable '", x, "' not found in the data frame.", 
             call. = FALSE)
      if (pos == 0L) 
        stop("Multiple matches for variable '", x, 
             "' in the data frame.", call. = FALSE)
      return(pos)
    })
    idvar <- (1:length(varnames))[unique(unlist(idvar.pos))]
  } else {
    idvar.pos <- unique(round(c(id, time)))
    if (min(idvar.pos) < 1 | max(idvar.pos) > nvars) { 
      stop("Variables positions must be between 1 and ", 
           nvars, ".")
    }
    idvar <- (1:length(varnames))[idvar.pos]
  }
  
  variables <- data[, -idvar]
  id        <- data[, idvar[1]]
  time      <- data[, idvar[2]]
  
  nsub <- nsub(id)
  
  # identify time-invariant variables
  tinvar.pos <- apply(variables, 2, function(x) check.timeinvar(x, id))
  tvars      <- data.frame(variables[, !tinvar.pos])
  tinvar     <- data.frame(variables[, tinvar.pos])
  
  # Get new discrete time variable given the tinterval
  timenew <- ceiling(time/tinterval)
  timemin <- min(timenew)
  timemax <- max(timenew)
  
  data.temp <- data.frame(matrix(NA, nsub * (timemax - timemin + 1), nvars))
  data.temp[, 1] <- rep(unique(id), each = (timemax - timemin + 1))
  data.temp[, 2] <- rep(timemin:timemax, nsub)
  
  for (i in 1:(dim(data.temp)[1])) {
    varying.scores <- tvars[(id == data.temp[i,1] & timenew == data.temp[i,2]),]
    if(length(unlist(varying.scores)) != 0) {
      data.temp[i, 3:(2 + dim(tvars)[2])] <- varying.scores
    }
    data.temp[i, (dim(tvars)[2] + 3):nvars] <- data.frame(tinvar[id == data.temp[i,1],])[1,]
  }
  
  data.temp[, 2] <- data.temp[, 2] * tinterval
  
  data.out <- data.frame(data.temp)
  names(data.out) <- c(varnames[idvar], names(variables)[!tinvar.pos], names(variables)[tinvar.pos])
  return(data.out)
}