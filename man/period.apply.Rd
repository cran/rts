\name{period.apply}

\alias{period.apply}
\alias{period.apply,RasterStackTS-method}
\alias{period.apply,RasterBrickTS-method}

\title{Apply a function over specified time intervals}

\description{
Apply specified function over each period of date/time defined in INDEX at each grid cell.
}

\usage{
period.apply(x, INDEX, FUN, ...) 
}

\arguments{
  \item{x}{ a raster time series (\code{Raster*TS}) object created by \code{\link[rts]{rts}} }
  \item{INDEX}{ a numeric vector of endpoints of time/date periods to apply function over }
  \item{FUN}{ an argument of type \code{function} }
  \item{\dots}{ additional arguments for \code{FUN}}
}
\details{
This functions subsets the raster data based on the specified time periods (endpoint for each period should be specified in \code{INDEX}), and \code{FUN} function will be applied to the subsetted values at each grid cell for each period. For each period, a raster will be calculated and the end of the date/time period will be assigned to it in the output raster time series object. If the \code{INDEX} is out of range, the function stops working and an error is generated.
}

\value{
RasterStackTS or RasterBrickTS
}

\author{Babak Naimi

\email{naimi@r-gis.net}

\url{http://r-gis.net}
}

\seealso{ \code{\link[xts]{period.apply}}}

\examples{
period.apply
}

\keyword{ utilities }