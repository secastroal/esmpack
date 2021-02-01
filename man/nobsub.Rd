\name{nobsub}
\alias{nobsub}
\title{Number of Observations per Subject}
\description{Function that retrieves the number of observations per subject of a dataset in long format.}
\usage{
nobsub(id, data)
}
\arguments{
   \item{id}{either a vector with the subjects' id or the name of the id variable.}
   \item{data}{optional data frame that contains the id variable.}
}
\details{
   The function computes how many observations there are per subject in a dataset in long format. It returns a one-column data frame, in which the row names are the subjects' id.
}
\value{
   A data frame.
}
\author{
   Sebastian Castro-Alvarez \email{secastroal@gmail.com} \cr
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, 1,2,1,3,1))

# compute number of observations with id as a vector
nobsub(dat$subj)

# compute number of observations with the id variable's name
nobsub(subj, dat)
}
\keyword{manip}
