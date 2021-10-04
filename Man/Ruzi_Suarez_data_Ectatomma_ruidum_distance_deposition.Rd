\name{Ruzi_Suarez_data_Ectatomma_ruidum_distance_deposition}
\alias{Ruzi_Suarez_data_Ectatomma_ruidum_distance_deposition}
\docType{data}
\title{
Ruzi_Suarez_data_Ectatomma_ruidum_distance_deposition
}
\description{
Observation data of how far Ectatomma ruidum moved seeds of Zanthoxylum ekmanii placed at five different sites over multiple replicates from two years of sampling. It also records the deposition location of the seeds, what movement number the observation was, and what color the seed was marked with. Only data from Ectatomma ruidum ants are included. Any site or seed that did not have a movement by Ectatomma ruidum was not retained in the dataset.
}
\usage{data("Ruzi_Suarez_data_Ectatomma_ruidum_distance_deposition")}
\format{
  A data frame with 51 observations on the following 10 variables.
  \describe{
    \item{\code{site}}{a categorical vector - the site at which the seed cache was placed. There are five sites at which seed caches were placed in both years sampled.}
    \item{\code{rep_num}}{a numeric vector - what replicate at that site the seed cache was. The replicate numbers repeat between years.}
    \item{\code{seed_color}}{a categorical vector - what color paint was applied to the seed in order to track individual seeds}
    \item{\code{distance_cm}}{a numeric vector - the straight line distance Ectatomma ruidum workers moved seeds}
    \item{\code{destination}}{a categorical vector - where the seed was deposited or whether the seed was lost}
    \item{\code{ant_species}}{a categorical vector - the ant species that moved the seed. In this dataset only Ectatomma ruidum is considered}
    \item{\code{num_foragers}}{a categorical vector - how many Ectatomma ruidum forgers or workers moved the seed. Typically only one worker is needed to move a seed but occasionally there were more than one.}
    \item{\code{movement_num}}{a categorical vector - which movement the observation belongs to. Can be one, two, or three as three was the highest movement recorded}
    \item{\code{year}}{a numeric vector - the year in which the sampling took place}
    \item{\code{ID}}{a categorical vector - combination of \code{year} and \code{movement_num} separated by a \code{.}}
  }
}
\details{
Used to determine number of Zanthoxylum ekmanii seeds moved, how far moved, and where moved to by Ectatomma ruidum.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_Suarez_data_Ectatomma_ruidum_distance_deposition)
str(Ruzi_Suarez_data_Ectatomma_ruidum_distance_deposition)
}
\keyword{datasets}
