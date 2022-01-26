\name{Ruzi_Suarez_data_seed_removal_2hours}
\alias{Ruzi_Suarez_data_seed_removal_2hours}
\docType{data}
\title{
Ruzi_Suarez_data_seed_removal_2hours
}
\description{
Seed removal data for three sampling years at five plots. Includes the date of the sampling event, how many seeds were placed, and the number of seeds removed as well as the randomly chosen edge of the plot sampled.}
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
Two hour seed removal information used to determine if seed removal differed over the years. It includes data reanalyzed from Ruzi et al. (2017,2021) that was collected in 2013 as well as the seed removal amounts from this study collected in 2015 and 2016. In 2013, caches consisted of 10 seeds each and there were additional caches of other tree species located at one meter intervals. No leaf litter was move and the area was not baited prior to sampling. In this study (2015 and 2016), caches consisted of 5 seeds. There were no other tree species represented at caches. A portion of the leaf litter was moved and the area was baited prior to sampling.
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_Suarez_data_seed_removal_2hours)
str(Ruzi_Suarez_data_seed_removal_2hours)
}
\keyword{datasets}
