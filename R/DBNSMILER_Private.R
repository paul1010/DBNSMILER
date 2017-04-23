# --------------------------------------------------------------------------------------------
# Private methods in DBNSMILER
# --------------------------------------------------------------------------------------------

#' @title .onLoad (PRIVATE)
#' @description This function executes on loading of the package
#' @param pkgname is the name of the package (DBNSMILER)
#' @param libname = is the location of the package (note that java libraries MUST BE in
#' /inst/java)
#' @return void
#' @export
#' @examples none
#' @name .onLoad
.onLoad = function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
}

#' @title initdbn PRIVATE
#' @description This function constructs the dbn structure used to store the DBN
#' @param net is the SMILE DBN java object
#' @param n is the SMILE node ID (integer) to the first node the DBN
#' @param xdslfilepath is a char of the filename and path to the SMILE .xdsl file
#' @return void
#' @export
#' @examples none
#' @name initdbn
initdbn = function(net,n, xdslfilepath) {
  # Step 1: Obtain the full nodelist from SMILE .xdsl file
  i=1
  nodelist=vector('integer',length=0)
  while(n>=0) {
    nodelist[i] = n
    i = i + 1
    n = .jcall(obj=net, returnSig='I', method='getNextNode',n)
  }
  
  # Step 2: Initialise data structure
  # --- Part A: network structure
  dbn = list()
  dbn$node = vector('list',length=length(nodelist))
  maxdtslices = getmaxdtslices # Max #dt slices in network (t, t+1 etc.)
  for(i in 1:length(dbn$node)) {
    dbn$node[[i]]$name = getnodename(net, nodelist[i])             # Node name
    dbn$node[[i]]$id = as.integer(nodelist[i])                     # Node id (int)
    dbn$node[[i]]$states = getnodestates(net, nodelist[i])         # Node states (labels)
    
    dbn$node[[i]]$nodenotes = ''                                   # Node notes
    dbn$node[[i]]$nodethresh = ''                                  # Node thresholds for plotting
    
    dbn$node[[i]]$dtslices= getnodedtslices(net, nodelist[i])#Vector of #node dtslices (t, t+1 etc.)
    dbn$node[[i]]$parnodenames = list() # Parent node names foreach dt slice
    dbn$node[[i]]$parnodeinds = list()  # Parent node indices (SMILE id - integer) foreach dt slice
    dbn$node[[i]]$parstates = list()    # Parent node states (vector of char) foreach dt slice
    dbn$node[[i]]$cpt = list()          # Node CPT (numeric matrix) foreach dt slice
    dbn$node[[i]]$formattedcpt = list() # Node CPT (formatted data frame) foreach dt slice
    dbn$node[[i]]$rules = vector('list',length=length(dbn$node[[i]]$dtslices)) # rules for this node
    
    for(j in 1:length(dbn$node[[i]]$dtslices)) { # For each dt slice
      x = getparents(net, dbn$node[[i]]$id, dbn$node[[i]]$dtslices[j])
      dbn$node[[i]]$parnodenames[[j]] = x$parnames
      dbn$node[[i]]$parnodeinds[[j]] = x$parinds
      if(length(x$parnames)>1) {
        dbn$node[[i]]$parstates[[j]] = getnodestates(net, dbn$node[[i]]$parnodeinds[[j]])
      } else {#getnodestates returns a vector if only 1 parent, so convert to a list for consistency
        dbn$node[[i]]$parstates[[j]] = list(getnodestates(net, dbn$node[[i]]$parnodeinds[[j]]))
      }
      n = dbn$node[[i]]
      dbn$node[[i]]$cpt[[j]] = getcpt(net, n$id, n$dtslices[j])
      dbn$node[[i]]$formattedcpt[[j]] = formatcpt(n$parnodenames[[j]],n$parstates[[j]],
                                                  dbn$node[[i]]$cpt[[j]],n$states)
      dbn$node[[i]]$rules[[j]] = initrules(n$name,n$states,n$parnodenames[[j]],n$parstates[[j]])
    } # End for each dt slice configuration foreach node
  } # End for each node
  
  # --- Part B: Inference and simulation aspect of DBN
  dbn$net = net         # SMILE DBN object (used for setting evid, inference etc.)
  dbn$datetype = 'none' # Date type: 'year','month','day','hour','minute','second','none'
  dbn$globdater = NA    # $char = 2 element vector of datetime, $num = 2-element datenum of datetime
                        # Evidence/scenarios
  dbn$evid = data.frame(EvidID=integer(0), Scenario=character(0), Node=character(0),
                        DateStart=character(0), DateEnd=character(0),
                        SelectState=character(0), SoftEvidence=character(0),stringsAsFactors=FALSE)
                        # Posteriors/results of simulation/inference
  dbn$posterior = data.frame(node=character(0),state=character(0),t=character(0),p=numeric(0),
                              scenario=integer(0),pctile=numeric(0),xbar=numeric(0),
                             ybar=numeric(0),py=numeric(0),
                             stringsAsFactors=FALSE) 
  dbn$lineplots = NA      # ggplot objects - lineplots of Pr foreach state over time
  dbn$contourplots = NA   # ggplot objects - contour plots of node values over time, uses nodethresh
  
  dbn$xdslfilepath = xdslfilepath # file name and path to source .xdsl file
  return(dbn)
}







