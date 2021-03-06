% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBNSMILER_Public_Get_Set.R
\name{getparents}
\alias{getparents}
\title{getparents}
\usage{
getparents(net, n, dtslice)
}
\arguments{
\item{net}{- SMILE net object}

\item{n}{- SMILE node id (integer)}

\item{dtslice}{- node dt slice (0 = same time slice, 1 = t+1 etc.)}
}
\value{
list$parinds = node parent indices as vector of ints, $parnames = parent names as vector of chars
}
\description{
Gets the node parents
}
\examples{
none
}

