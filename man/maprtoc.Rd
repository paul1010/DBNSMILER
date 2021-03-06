% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBNSMILER_Public_Network.R
\name{maprtoc}
\alias{maprtoc}
\title{maprtoc}
\usage{
maprtoc(rules, tblcpt, pnames, states)
}
\arguments{
\item{rules}{table as a data frame - format of rules table is as described in 'initrules'}

\item{pnames}{vector of parent node names (char)}

\item{states}{vector of node state labels (char)}

\item{CPT}{table data frame (formatted using 'formatcpt')}
}
\value{
formatted CPT updated with rules. If error occurs, returns original tblcpt
}
\description{
Map rules to node CPT
}
\examples{
none
}

