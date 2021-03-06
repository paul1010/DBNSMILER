% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBNSMILER_Public_InfVis.R
\name{fastwmean}
\alias{fastwmean}
\title{fastwmean}
\usage{
fastwmean(posterior, dbn, pctiles)
}
\arguments{
\item{posterior}{- data frame formatted as per dbn$posterior (see DBNSMILER-package 
documentation); $node,$state,$t,$p,$scenario,$pctile,$xbar}

\item{dbn}{DBNSMILER object (see DBNSMILER-package documentation). Use dbn to match node 
names to node state thresholds & states needed to determine expected value assuming uniform
distributions.}

\item{pctiles-}{numeric vector of percentiles for evaluating expected value=weighted mean at}
}
\value{
posterior data frame (see DBNSMILER-package documentation) with expected val/weighted
mean results added; if error, original posterior data frame is returned. Note that all existing
expected val/weighted mean results in posterior are untouched.
}
\description{
Calculates the weighted mean from posterior probabilities assuming a uniform
distribution across states, at specified percentiles. The results are added to the 
posterior object. This fast version ASSUMES posterior data frame follows a consistent order
w.r.t. results - i.e. sorted by scenarios, then, nodes, then time, then node states as
put together by getposterior().
}
\examples{
none
}

