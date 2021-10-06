\name{Ruzi_Suarez_data_overall_seed_removal_2hours}
\alias{Ruzi_Suarez_data_overall_seed_removal_2hours}
\docType{data}
\title{
Ruzi_Suarez_data_overall_seed_removal_2hours
}
\description{
Seed removal data for two sampling years at five plots. Includes the date of the sampling event, how many seeds were placed, and the number of seeds removed as well as the randomly chosen edge of the plot sampled.}
\usage{data("Ruzi_Suarez_data_overall_seed_removal_2hours")}
\format{
  A data frame with 25 observations on the following 8 variables.
  \describe{
  \item{\code{Plot}}{a categorical vector - the plot at which the sampling event took place}
  \item{\code{Year}}{a numerical vector - the year in which the sampling event took place}
  \item{\code{Rep}}{a numerical vector - what replicate the sample event was. Numbers repeat over the years}
  \item{\code{Side}}({a categorical vector - the randomly chosen side of the plot sampled}
  \item{\code{Seeds_placed}}{a numerical vector - the number of seeds initially placed}
  \item{\code{Seeds_removed}}{a numerical vector - the number of seeds removed by the end of the 2 hour sampling period}
  \item{\code{Seeds_remaining}}{a numerical vector - the number of seeds that were still located at the original deposition location at the end of the 2 hours}
  \item{\code{Date}}{a categorical vector - the date the sampling event took place}
  }
}
\details{
Used to determine the amount of seeds removed over two hours in the two sampling years.}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_Suarez_data_overall_seed_removal_2hours)
str(Ruzi_Suarez_data_overall_seed_removal_2hours)
}
\keyword{datasets}
