\name{split.esm}
\alias{split.esm}
\title{Split ESM per Subject}
\description{Function that splits the data creating a data frame for each subject.}
\usage{
split.esm(data, id, include = NULL, exclude = NULL)
}
\arguments{
   \item{data}{data frame to split.}
   \item{id}{vector to specify the subject id variable.}
   \item{include}{optional character vector (giving the names of the variables to include in the individual data frames) or a numeric vector (giving the position of the columns in the data frame corresponding to the variables). It defaults to \code{NULL} so all variables are included.}
   \item{exclude}{optional character vector (giving the names of the variables to exclude from the individual data frames) or a numeric vector (giving the position of the columns in the data frame corresponding to the variables). It defaults to \code{NULL} so all variables are included.}
}
\details{
   The function splits the data into a list, where each element is a data frame with the data of one subject.
   
   The arguments \code{include} and \code{exclude} are used to specify which variables to include in the final data frames. Only one of these arguments should be provided. If both arguments are provided, the argument \code{exclude} is ignored.
}
\value{
   A list of data frames, where each data frame corresponds to the data of one subject.
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

# split data
split.esm(dat, dat$subj)

# split data including variables age and stress by indicating their column numbers
split.esm(dat, subj, include = 3:4)

# split data excluding the variable subj  
split.esm(dat, subj, exclude = "subj")
}
\keyword{manip}
