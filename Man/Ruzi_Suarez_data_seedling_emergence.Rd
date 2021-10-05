\name{Ruzi_Suarez_data_chamber_depths}
\alias{Ruzi_Suarez_data_chamber_depths}
\docType{data}
\title{
Ruzi_Suarez_data_chamber_depths
}
\description{
Surface area of Zanthoxylum ekmanii seeds that were or were not damaged. These seeds either presumably never came into contact with ants or were collected from Ectatomma ruidum colonies.
}
\usage{data("Ruzi_Suarez_data_chamber_depths")}
\format{
  A data frame with 38 observations on the following 8 variables.
  \describe{
  \item{\code{Depth_cm}}{a numerical vector - the depth the seeds were buried at for this replicate}
  \item{\code{Replicate}}{a numerical vector - which replicate number this burial depth is}
  \item{\code{Num_seeds_buried}}{a numerical vector - the number of seeds buried at this depth for this replicate}
  \item{\code{Num_seeds_emerged}}{a numberial vector - the number of seedlings that were able to emerge at this depth for this replicate}
  \item{\code{Prop_emerged}}{a numerical vector - the proportion of seedlings emerged to those placed}
  \item{\code{Mass_soil}}{a numerical vector - the mass of the soil added to the clear tube not including the mass of the tube itself}
  \item{\code{Soil_autoclaved}}{a categorical vector - whether the soil was sterilized or not}
  \item{\code{Holes}}{a categorical vector - whether the tube had some pin holes punctured into it}
  }
}
\details{
Used to determine if burial depth impacted seedling emergence and to assess whether if seeds were cached within an Ectatomma ruidum colony and germinated if they would survive.}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_Suarez_data_chamber_depths)
str(Ruzi_Suarez_data_chamber_depths)
}
\keyword{datasets}
