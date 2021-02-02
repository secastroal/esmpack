\name{check.conflict}
\alias{check.conflict}
\title{Check If Two Opposite Variables are Too Similar to Each Other}
\description{Function that checks if the values of two opposite variables are too similar for each row in the data.}
\usage{
check.conflict(x, y, data, threshold, out=1)
}
\arguments{
   \item{x, y}{vectors to specify the two opposite variables to check.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{threshold}{scalar that indicates the maximum size of the difference between two values that is considered to be too similar.}
   \item{out}{either a string or an integer (\code{1} = \code{"logical"}, \code{2} = \code{"ind"}, \code{3} = \code{"data"}) indicating what information should be returned (Default is \code{1}).}
}
\details{
   The function checks if the values of two opposite variables are too similar to each other.
}
\value{
   When \code{out = 1} or \code{out = "logical"}, the function simply returns a logical vector indicating whether the values are too similar (\code{TRUE}) or not (\code{FALSE}).

   When \code{out = 2} or \code{out = "ind"}, the function returns a vector with the row indexes where the values of the two variables were too similar to each other.

   When \code{out = 3} or \code{out = "data"}, the function returns the data for those rows where the the two variables were too similar to each other.
}
\author{
   Sebastian Castro-Alvarez \email{secastroal@gmail.com} \cr
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,1,4,2, 3,3,3,3,3, 1,1,2,5,4, 1,2,1,3,1),
                  relax  = c(5,4,5,1,4, 3,3,2,3,4, 4,2,4,3,4, 3,3,4,3,5))

# check observations for which stress and relax were too similar
check.conflict(stress, relax, data=dat, threshold = 1)

# for which rows are the values of stress and relax too similar?
check.conflict(stress, relax, data=dat, threshold = 1, out = 2)

# show the data for those observations
check.conflict(stress, relax, data=dat, threshold = 1, out = 3)
}
\keyword{manip}
