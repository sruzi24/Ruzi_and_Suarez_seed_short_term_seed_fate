\name{Ruzi_Suarez_data_seed_surface_area}
\alias{Ruzi_Suarez_data_seed_surface_area}
\docType{data}
\title{
Ruzi_Suarez_data_seed_surface_area
}
\description{
Surface area of Zanthoxylum ekmanii seeds that were or were not damaged. These seeds either presumably never came into contact with ants or were collected from Ectatomma ruidum colonies.
}
\usage{data("Ruzi_Suarez_data_seed_surface_area")}
\format{
  A data frame with 77 observations on the following 16 variables.
  \describe{
  \item{\code{Seed_species}}{a categorical vector - the seed species. In this case only Zanthoxylum ekmanii}
  \item{\code{file_name}}{a categorical vector - the name of the associated image of the seed}
  \item{\code{Control_Colony}}{a categorical vector - whether the seed was a control (i.e. presumably has never interacted with ants)or what colony the seed was retrieved from}
  \item{\code{groupID}}{a numerical vector - the replicate number. Resets between the two treatments}
  \item{\code{Chamber}}{a categorical vector - what chamber the seed was retrieved from if retrieved from a colony or "control" if not retrieved from a colony}
  \item{\code{total_SA}}{a numerical vector - the total surface area of the seed in the image}
  \item{\code{Darea_1}}{a numerical vector - first damaged surface area measured on the seed}
  \item{\code{D_area2}}{a numerical vector - second damaged surface area measured on the seed}
  \item{\code{D_area3}}{a numerical vector - third damaged surface area measured on the seed}
  \item{\code{D_area4}}{a numerical vector - fourth damaged surface area measured on the seed}
  \item{\code{D_area5}}{a numerical vector - fifth damaged surface area measured on the seed}
  \item{\code{Included_good_areas}}{a numerical vector - any non-damaged surface areas located within the dramaged areas on the seed}
  \item{\code{Sum_damage}}{a numerical vector - sum of all the damaged surface areas on the seed}
  \item{\code{Seed_damage_percent}}{a numerical vector - percentage of the total surface area of the seed that has been damaged}
  \item{\code{notes1}}{a categorical vector - first set of notes by SAR}
  \item{\code{notes2}}{a categorical vector - some additional notes by SAR}
  }
}
\details{
Used to determine whether Ectatomma ruidum workers damage Zanthoxylum ekmanii seeds.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_Suarez_data_seed_surface_area)
str(Ruzi_Suarez_data_seed_surface_area)
}
\keyword{datasets}
