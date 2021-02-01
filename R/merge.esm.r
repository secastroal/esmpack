merge.esm <- function(vars, id, esmdata, crossdata) {
  if (is.null(esmdata)) {
    stop("Argument 'esmdata' must be specified.")
  }
  if (is.null(crossdata)) {
    stop("Argument 'crossdata' must be specified.")
  }
  if (is.null(vars)) {
    stop("Argument 'vars' must be specified.")
  }
  if (is.null(id)) {
    stop("Argument 'id' must be specified.")
  }
  if (!is.data.frame(esmdata)) {
    esmdata <- data.frame(esmdata)
  }
  if (!is.data.frame(crossdata)) {
    crossdata <- data.frame(crossdata)
  }
  
  if (!(length(id) %in% 1:2)) {
    stop("Argument 'id' must either be a character or a numeric vector of length 1L or 2L.")
  }
  
  if (length(id) == 1) {
    id <- rep(id, 2)
  }
  
  esmvarnames <- names(esmdata)
  esmnvars    <- length(esmvarnames)
  
  varnames    <- names(crossdata)
  nvars       <- length(varnames)
  
  if (!(is.character(id) | is.numeric(id))) 
    stop("Argument 'id' must either be a character or a numeric vector.")
  if (is.character(id)) {
    esm.id.pos <- charmatch(id[1], esmvarnames)
    if (length(esm.id.pos) == 0L) {
      stop("Variable '", id[1], "' not found in the longitudinal data frame.",
           call. = FALSE)
    }
    cross.id.pos <- charmatch(id[2], varnames)
    if (length(cross.id.pos) == 0L) {
      stop("Variable '", id[2], "' not found in the cross-sectional data frame.",
           call. = FALSE)
    }
  } else {
    esm.id.pos <- round(id[1])
    if (min(esm.id.pos) < 1 | max(esm.id.pos) > esmnvars) { 
      stop("ID longitudinal variable position must be between 1 and ", 
           esmnvars, ".")
    }
    cross.id.pos <- round(id[2])
    if (min(cross.id.pos) < 1 | max(cross.id.pos) > nvars) { 
      stop("ID cross-sectional variable position must be between 1 and ", 
           nvars, ".")
    }
  }
  
  if (!(is.character(vars) | is.numeric(vars))) 
    stop("Argument 'vars' must either be a character or a numeric vector.")
  if (is.character(vars)) {
    cross.vars.pos <- lapply(vars, function(x) {
      pos <- charmatch(x, varnames)
      if (is.na(pos)) 
        stop("Variable '", x, "' not found in the data frame.", 
             call. = FALSE)
      if (pos == 0L) 
        stop("Multiple matches for variable '", x, 
             "' in the data frame.", call. = FALSE)
      return(pos)
    })
    cross.vars.pos <- unique(unlist(cross.vars.pos))
  } else {
    cross.vars.pos <- unique(round(vars))
    if (min(cross.vars.pos) < 1 | max(cross.vars.pos) > nvars) { 
      stop("Variables positions must be between 1 and ", 
           nvars, ".")
    }
  }
  esmid     <- esmdata[, esm.id.pos]
  crossvars <- crossdata[, c(cross.id.pos, cross.vars.pos)]
  
  observations <- nobsub(esmid)
  
  temp <- data.frame(matrix(NA, nrow = nsub(esmid), ncol = 1 + length(vars)))
  colnames(temp) <- c("id", varnames[cross.vars.pos])
  temp[, 1] <- unique(esmid)
  
  for (i in 1:(dim(temp)[1])) {
    subj <- temp[i, 1]
    scores <- crossvars[crossvars[, 1] == subj, 2:(length(vars) + 1)]
    if (length(unlist(scores)) == 0L) {
      temp[i, 2:(length(vars) + 1)] <- rep(NA, length(vars))
    } else {
      temp[i, 2:(length(vars) + 1)] <- scores
    }
  }
  rm(i)
  
  observations.match <- observations[match(temp[, 1], row.names(observations)), ]
  
  new.vars <- temp[rep(1:(dim(temp)[1]), times = observations.match), 2:(length(vars) + 1)]
  
  out <- data.frame(esmdata, new.vars)
  
  if (length(vars) == 1) {
    names(out)[dim(out)[2]] <- varnames[cross.vars.pos]
  }
  
  row.names(out) <- 1:dim(out)[1]
  
  return(out)
}
