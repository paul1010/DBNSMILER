#' DBNSMILER.
#'
#' @name DBNSMILER
#' @docType package
#' @description This package is a pseudo-Object-Oriented Dynamic Bayesian Network (DBN) modelling library. 
#' It provides a R-interface to SMILE to edit CPTs, set evidence and run inference.
#' It also provides a means for manipulating rules and CPTs. 
#' 
#' The DBN data structure is as follows:
#' \itemize{
#'  \item $node[[i]]
#'    \itemize{
#'      \item $name (char name)
#'      \item $id (int)
#'      \item $states (vector of char state labels)
#'      \item $notes (char of node notes)
#'      \item $nodethresh (char of node threhsolds for plotting)
#'      \item $dtslices (vector of dt slices t, t+1 etc.)
#'      \item $parnodenames (parent node names foreaach dt slice - [[dtslice]][nodename])
#'      \item $parnodeinds (same as above but with parent integer indices [[dtslice]][nodeind])
#'      \item $parstates (parent node states vector of char foreach dt slice [[dtslice]][nodestatelabel])
#'      \item $cpt (numeric matrix of CPT foreach [[dtslice]])
#'      \item $formattedcpt (formatted CPT with row/col headers as a data frame foreach [[dtslice]])
#'      \item $rules (rules tables foreach dt slice [[dtslice]][char data frame of rules])
#'    }
#'  \item $net (SMILE DBN object - used for setting evid, inference etc.)
#'  \item $datetype ('year','month','day','hour','minute','second','none')
#'  \item $globdater ($char = 2 element vector of datetime, $num = 2-element datenum of datetime)
#'  \item $evid (data frame: EvidID=int, Scenario=char, Node=char, DateStart=char, DateEnd=char, SelectState=char, SoftEvidence=char)
#'  \item $posterior (data frame: node=char,state=char,t=char,p=numeric,scenario=int,
#'  pctile=numeric,xbar=numeric expected value for given percentile,ybar=observed numeric exp. val
#'  for given percentile, py = numeric observed probability)
#'  \item $lineplots (ggplot objects)
#'  \item $contourplots (ggplot objects)
#'  \item $xdslfilepath char of file name and path to source SMILE .xdsl file
#'  }
#'  
#' @note This package uses a custom compiled version of jSMILE (https://dslpitt.org/genie/)
#' that exposes the SMILE C++ functions for manipulating temporal virtual evidence.
#' 
#' The DBFIVE data structure (made via dbn2DFnodes) is as follows:
#' \itemize{
#'  \item $nnames (char vec of node names),
#'  \item $statenames (char vec odf node state names),
#'  \item $parnodeind ([[1..maxdt,node]][parent node ind to nnames]),
#'  \item $parnodetslice ([[1..maxdt,node]][par k for $\Pi(t-k)$]), 
#'  \item $childnodeind ([[1..maxdt,node]][child node ind to nnames])
#'  \item $childnodetslice ([[1..maxdt,node]][child k for $\Pi(t+k)$]), 
#'  \item $numstates (vec of number of node states),
#'  \item $cpt ([[1..maxdt,node]][numeric vector of CPT])
#'  \item $maxdt = (int) max# dt-slices (e.g. Markov network, maxdt = 1; BN, maxdt=0)
#'  \item $tend = index to last t-slice in simulation window 1:tend
#'  \item $evid_t [[1..tend,node]][vector of len numstates containing node marginals]
#'  \item $nodecost[1..maxdt,node] containing cost value $OC=D+C$
#'  \item $nodedist[1..maxdt,node] containing distance $D$
#'  \item $parpars
#'    \itemize{
#'      \item $n[[k=1..maxdt for $\Pi(t-k)$,node]] vector of node inds
#'      \item $t[[k=1..maxdt for $\Pi(t-k)$,node]] vector of node dt-slice t-k
#'    }
#'  \item $nodeorder
#'    \itemize{
#'      \item $n[[k=1..maxdt for $\Pi(t-k)$,target node]] vector of node inds in order to eliminate
#'      \item $t[[k=1..maxdt for $\Pi(t-k)$,target node]] vector of node dt-slice t-k
#'    }
#'  \item $lknodeorder - like nodeorder but for link nodes $\pi(t)$
#'    \itemize{
#'      \item $n[[k=1..maxdt for $\Pi(t-k)$,target node]] vector of node inds in order to eliminate
#'      \item $t[[k=1..maxdt for $\Pi(t-k)$,target node]] vector of node dt-slice t-k
#'    }
#'  \item $nodeordch[[k=1..maxdt for $\Pi(t-k)$,target node]][[foreach nodeorder node]][vec of 
#'  childnodeind]
#'  \item $nodeordcht[[k=1..maxdt for $\Pi(t-k)$,target node]][[foreach nodeorder node]][vec of 
#'  child dt-slice k as t+k]
#'  \item $cptaddch[[k=1..maxdt for $\Pi(t-k)$,target node]][[foreach nodeorder node]][vec of 
#'  nodeind corresponding to CPT's to add for multiply step]
#'  \item $cptaddcht[[k=1..maxdt for $\Pi(t-k)$,target node]][[foreach nodeorder node]][vec of node
#' dt-slice corresponding to CPT's to add for multiply step]
#'  \item $lkcptaddch[[k=1..maxdt for $\Pi(t-k)$,target node]][[foreach nodeorder node]][vec of 
#'  nodeind corresponding to CPT's to add for multiply step] - for link nodes $\pi(t)$
#'  \item $lkcptaddcht[[k=1..maxdt for $\Pi(t-k)$,target node]][[foreach nodeorder node]][vec of 
#'  node dt-slice corresponding to CPT's to add for multiply step] - for link nodes $\pi(t)$
#' \item $targetnodeord is a vector containing target nodes $\mathcal{T}$ in the order to be evaled
#' \item J[[1..tend,target node]] - Joint structure
#'    \itemize{
#'      \item $p = numeric vector of p
#'      \item $n = int vector of nodes n in $J$ in order in which $J$ is composed
#'      \item $t = int vector of node t-slices 1..tend
#'      \item $s = list of state-labels (as indices) foreach node in $n$
#'    }
#'  \item Jlink[[1..tend,target node]] is the joint for $\pi(t)$ (same struct as J)
#'  \item Jcpt[[1..tend,target node]] is the original joint populated by $cpt (same struct as J)
#' }
NULL
