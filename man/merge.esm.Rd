\name{merge.esm}
\alias{merge.esm}
\title{Merge Longitudinal and Cross-Sectional Data}
\description{Function that merges a cross-sectional data frame with a longitudinal data frame in long format.}
\usage{
merge.esm(vars, id, esmdata, crossdata)
}
\arguments{
   \item{vars}{a character vector (giving the names of the cross-sectional variables to be merged) or a numeric vector (giving the position of the columns in the cross-sectional data frame corresponding to the variables to be merged).}
   \item{id}{a character vector (giving the names of the id variables in the two data frames) or a numeric vector (giving the position of the columns in the two data frames corresponding to the id variables) of length \code{1L} or \code{2L}. See \sQuote{Details}}
   \item{esmdata}{longitudinal data frame in long format.}
   \item{crossdata}{cross-sectional data frame that contains the variables specified in \code{vars} to be merged with the longitudinal data frame.}
}
\details{
   The function merges the specified variables in the cross-sectional data frame with the longitudinal data frame in long format. It adds the specified cross-sectional variables as new variables in the longitunidal data frame.  
   
   Not all the subjects in the longitudinal data frame need to be in the cross-sectional data frame or vice versa. If a subject is not found in the cross-sectional data frame, its values in the new variables are \code{NA}.  
   
   The argument \code{id} specifies the name or the column number of the id variables in the two data frames. The first element is the name or the column number of the id variable in the longitudinal data frame. The second element is the name or the column number of the id variable in the cross-sectional data frame. If \code{id} is of length \code{1L}, it is assumed that the id variables are named equally or are on the same column number in the two data frames to merge.
}
\value{
   A data frame of longitudinal data in long format with the cross-sectional variables as new variables.
}
\author{
   Sebastian Castro-Alvarez \email{secastroal@gmail.com} \cr
}
\examples{
# illustrative longitudinal dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, 1,2,1,3,1))

# illustrative cross-sectional dataset
cross <- data.frame(idno  = 2:5,
                    sex   = rep(c("male", "female"), 2),
                    neuro = c(14, 65, 48, 53))

# merge the variable "sex" into the longitudinal data frame
mdat <- merge.esm(vars = "sex", id = c("subj", "idno"), esmdata = dat, crossdata = cross)
mdat

# merge the variables "sex" and "neuro" into the longitudinal data frame
mdat <- merge.esm(vars = 2:3, id = 1, esmdata = dat, crossdata = cross)
mdat

}
\keyword{manip}
