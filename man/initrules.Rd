% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBNSMILER_Public_Network.R
\name{initrules}
\alias{initrules}
\title{initrules}
\usage{
initrules(nodename, states, parnames, parstates)
}
\arguments{
\item{nodename}{- name of node (char)}

\item{states}{- vector of node state labels (char)}

\item{parnames}{- vector of parent node names (char)}

\item{parstates}{- list of vectors of char parent state names}
}
\value{
node rules table with headers added (first 2 rows). These are made up of: col1 = 
rule id# (int), cols foreach parent node (antecedents, char), cols foreach node state (char), and
1 col for rule priority.
}
\description{
Initialises the rules table headers for given node
}
\examples{
none
}

