\name{calc.mcent.ti}
\alias{calc.mcent.ti}
\title{Calculate the Mean-Centered Values of a Time-Invariant Variable}
\description{Function that calculates the mean-centered values of a particular time-invariant variable.}
\usage{
calc.mcent.ti(x, id, data, expand = TRUE)
}
\arguments{
   \item{x}{vector to specify the time-invariant variable.}
   \item{id}{vector to specify the subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{expand}{logical indicating whether to expand the returned vector so it can be added to the dataset as a new vairable (default is \code{TRUE}).}
}
\details{
   The function calculates the mean-centered values of the specified time-invariant variable.
   
   By default \code{expand = TRUE}, this means that the function returns a vector where the mean-centered value of the time-invariant variable is repeated per person in such a way that it can be added back to the dataset as a new variable. 
   
   If \code{expand = FALSE}, the function returns a vector with the mean-centered values of the time-invariant variable with a length equal to the number of subjects.
}
\value{
   A vector.
}
\author{
   Sebastian Castro-Alvarez \email{secastroal@gmail.com} \cr
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, 1,2,1,3,1))

# calculate the mean-centered values of age per person without repeating
calc.mcent.ti(dat$age, dat$subj, expand = FALSE)

# calculate the mean-centered values of age and add them as a new variable 
dat$c.age <- calc.mcent.ti(age, subj, data = dat)

# there can be missing values in the time-invariant variable
dat$age[c(6:10, 16)] <- NA
dat$c.age.na <- calc.mcent.ti(age, subj, data = dat)
}
\keyword{manip}
