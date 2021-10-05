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
  A data frame with 78 observations on the following 30 variables.
  \describe{
  \item{\code{Chamber}}{a numerical vector - which chamber in the wax caste the data is referring to. Shallower chambers have lower numbers}
  \item{\code{Depth}}{a numerical vector - the depth of the chamber. Chamber depths were measured as the distance from the soil surface to the bottom of the chamber.}
  \item{\code{Colony}}{a numerical vector - what colony the data is from}
  \item{\code{Egg.percent}}{a numerical vector - what percentage of the eggs found in the colony are found in that chamber}
  \item{\code{Egg.present}}{a numerical vector - whether eggs were present in that chamber}
  \item{\code{Egg.num}}{a numerical vector - how many eggs were present in that chamber}
  \item{\code{Larvae.percent}}{a numerical vector - what percentage of the larvae found in the colony are found in that chamber}
  \item{\code{Larvae.present}}{a numerical vector - whether larvae were present in that chamber}
  \item{\code{Larvae.num}}{a numerical vector - how many larvae were present in that chamber}
  \item{\code{Pupae.percent}}{a numerical vector - what percentage of the pupae found in the colony are found in that chamber}
  \item{\code{Pupae.present}}{a numerical vector - whether pupae were present in that chamber}
  \item{\code{Pupae.num}}{a numerical vector - how many pupae were present in that chamber}
  \item{\code{Worker.percent}}{a numerical vector - what percentage of the workers found in the colony are found in that chamber}
  \item{\code{Worker.present}}{a numerical vector - whether workers were present in that chamber}
  \item{\code{Worker.num}}{a numerical vector - how many workers were present in that chamber}
  \item{\code{Queen.percent}}{a numerical vector - what percentage of the queens found in the colony are found in that chamber}
  \item{\code{Queen.present}}{a numerical vector - whether queens were present in that chamber}
  \item{\code{Queen.num}}{a numerical vector - how many queens were present in that chamber}
  \item{\code{Male.percent}}{a numerical vector - what percentage of the males found in the colony are found in that chamber}
  \item{\code{Male.present}}{a numerical vector - whether males were present in that chamber}
  \item{\code{Male.num}}{a numerical vector - how many males were present in that chamber}
  \item{\code{Alate.percent}}{a numerical vector - what percentage of the alates found in the colony are found in that chamber}
  \item{\code{Alate.present}}{a numerical vector - whether alates were present in that chamber}
  \item{\code{Alate.num}}{a numerical vector - how many alates were present in that chamber}
  \item{\code{Seed.percent}}{a numerical vector - what percentage of the seeds (not Zanthoxylum ekmanii) found in the colony are found in that chamber}
  \item{\code{Seed.present}}{a numerical vector - whether seeds (not Zanthoxylum ekmanii) were present in that chamber}
  \item{\code{Seed.num}}{a numerical vector - how many seeds (not Zanthoxylum ekmanii) were present in that chamber}
  \item{\code{ZAEKseed.percent}}{a numerical vector - what percentage of the Zanthoxylum ekmanii seeds found in the colony are found in that chamber}
  \item{\code{ZAEKseed.present}}{a numerical vector - whether Zanthoxylum ekmanii seeds were present in that chamber}
  \item{\code{ZAEKseed.num}}{a numerical vector - how many Zanthoxylum ekmanii seeds were present in that chamber}
  }
}
\details{
Used to determine how deep chambers are within a colony, where different ant life stages are found within the colony, where seeds are found in the colony, and whether seeds would potentially be able to germinate and survive if buried within the colony.
}
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
