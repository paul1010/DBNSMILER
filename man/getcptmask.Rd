% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBNSMILER_Public_InfVis.R
\name{getcptmask}
\alias{getcptmask}
\title{getcptmask}
\usage{
getcptmask(rules, tblcpt, pnames, states)
}
\arguments{
\item{rules}{table as a data frame - format of rules table is as described in 
'initrules'/DBNSMILER}

\item{pnames}{vector of parent node names (char)}

\item{states}{vector of node state labels (char)}

\item{CPT}{table data frame (formatted using 'formatcpt' - see DBNSMILER)}
}
\value{
mask identifying which columns in CPT correspond to rule i; mask is list of length = 
unique(rules) and each element is vector mask of length ncol(cpt)-1
}
\description{
Get rule 'activation strength'/probability mass over time
}
\examples{
none
}

