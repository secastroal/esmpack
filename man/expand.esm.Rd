\name{expand.esm}
\alias{expand.esm}
\title{Expand ESM Data Given a Time Interval}
\description{Function that expands a dataset, by adding rows with missing values, given an specific time interval. }
\usage{
expand.esm(data, id, time, tinterval = 1, include = NULL, exclude = NULL)
}
\arguments{
   \item{data}{data frame to expand.}
   \item{id}{either the character name or the number of the column of the subject id variable.}
   \item{time}{either the character name or the number of the column of the variable that should be used as 'time'.}
   \item{tinterval}{scalar to specify the time interval that is used to expand the dataset. It defaults to 1.}
   \item{include}{optional character vector (giving the names of the variables to include in the final data frame) or a numeric vector (giving the position of the columns in the data frame corresponding to the variables). It defaults to \code{NULL} so all variables are included.}
   \item{exclude}{optional character vector (giving the names of the variables to exclude from the final data frame) or a numeric vector (giving the position of the columns in the data frame corresponding to the variables). It defaults to \code{NULL} so all variables are included.}
}
\details{
   The function adds rows with missing values for the time-varying variables given a specific time interval. Time invariant variables are automatically identified and their values are repeated for the additional rows.
   
   If the time interval is too large given the scale of the time variable, multiple observations can be included in one time interval. In these cases, only the first observation within that time interval is taken into account. Warning messages are provided, which specify the persons' IDs and the time intervals when this happened.
   
   The arguments \code{include} and \code{exclude} are used to specify which variables to include in the final data frame. Only one of these arguments should be provided. If both arguments are provided, the argument \code{exclude} is ignored.
}
\value{
   A data frame with additional rows with missing values.
}
\author{
   Sebastian Castro-Alvarez \email{secastroal@gmail.com} \cr
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  hour   = c(8, 11, 14, 17, 20, 
                             7, 10, 13, 16, 19, 
                             8, 10, 12, 15, 20, 
                             9, 12, 15, 18, 21),
                  sex    = rep(c("male", "female"), each = 10),
                  age    = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, NA,2,1,3,1),
                  relax  = c(5,4,NA,1,4, 3,3,NA,3,NA, 4,2,4,3,4, NA,3,4,3,5))

# remove rows with missing values 
dat <- na.omit(dat)
dat

# add NAs rows to have 5 observations per individual
expand.esm(data = dat, id = "subj", time = "obs")

# add NAs rows including only the variables 'stress' and 'relax' 
expand.esm(data = dat, id = "subj", time = "obs", 
           include = c(6, 7))

# add NAs rows getting an observation for each hour and
# excluding the variables 'obs', 'sex', and 'age'.
expand.esm(data = dat, id = 1, time = 3, 
           exclude = c(2, 4, 5))

# add NAs rows with time intervals of 2 hours.
expand.esm(data = dat, id = 1, time = 3, tinterval = 2)

# add NAs rows with time intervals of 2.5 hours.
expand.esm(data = dat, id = "subj", time = "hour", tinterval = 2.5, 
           exclude = c("obs"))
}
\keyword{internal}
