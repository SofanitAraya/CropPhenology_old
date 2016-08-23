\name{CropPhenology-package}
\alias{CropPhenology-package}
\alias{CropPhenology}
\docType{package}
\title{
\packageTitle{CropPhenology}
}

\details{
Remotely sensed vegetation indexes to measure crop growth through phenological metrics have huge potential for agricultural management. Plotting the series of vegetation Index values over time produces a curve that highlights the NDVI dynamics associated with crop growth over the season. Metrics which are derived from the curve indicate important phenologic events of the crop. The indicative metrics have been used in a number of crop monitoring applications including  crop type mapping and classification (Zhong et al., 2011), regional yield estimation (Sakamoto et al., 2013), and yield forecasting (Bolton and Friedl, 2013).

  \figure{Figure1}
  
CropPhenology package extracts fifteen phenologic metrics from the time-series vegetation index data (Figure1). It takes raster vegetation index images input with very minimum pre-processing steps and produce easy interpreatable and integratable raster outputs. CropPhenology also provides a function to plot the dynamics curves from selected pixels, which enable visualization of the relative variability of crop growth among the selected locations.   
  
}

\author{
\packageAuthor{CropPhenology}

}
\references{
{Henebry, G., de Beurs, K., 2013. Remote Sensing of Land Surface Phenology: A Prospectus, in: Schwartz, M.D. (Ed.), Phenology: An Integrative Environmental Science. \emph{Springer Netherlands, pp. 385-411.}}

{Zhong, L., Hawkins, T., Biging, G., Gong, P., 2011. A phenology-based approach to map crop types in the San Joaquin Valley, California.\emph{International Journal of Remote Sensing,} \bold{32, 7777-7804.}}

{Sakamoto, T., Gitelson, A.A., Arkebauer, T.J., 2013. MODIS-based corn grain yield estimation model incorporating crop phenology information.\emph{Remote Sensing of Environment,}\bold{131, 215-231.}}

{Bolton, D.K., Friedl, M.A., 2013. Forecasting crop yield using remotely sensed vegetation indices and crop phenology metrics. \emph{Agricultural and Forest Meteorology,}\bold{173, 74-84.}}
}
\keyword{ package }
