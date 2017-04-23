# --------------------------------------------------------------------------------------------
# Get and Set Public methods in DBNSMILER
# --------------------------------------------------------------------------------------------

#' @title getnodename 
#' @description Gets the node name 
#' @param net - SMILE net object
#' @param n - SMILE node id (integer)
#' @return node name as char string
#' @export
#' @examples none
#' @name getnodename
getnodename = function(net, n) {
  return( .jcall(obj=net, returnSig='S',method='getNodeId',as.integer(n))) 
}

#' @title getnodestates 
#' @description Gets the node states - this function can handle vector of nodes or empty n
#' @param net - SMILE net object
#' @param n - SMILE node id (integer or vector of integers)
#' @return node states as a vector of chars (if n is one node), or a list of vector of chars (when
#' n is a vector of nodes) - each list item corresponds to a node in n
#' @export
#' @examples none
#' @name getnodestates
getnodestates = function(net, n) {
  if(length(n)==1) {
    return( .jcall(obj=net, returnSig='[S', method='getOutcomeIds', as.integer(n)) )
  } else if(length(n)>1) {
    y = list()
    for(i in 1:length(n)) {
      y[[i]] = .jcall(obj=net, returnSig='[S', method='getOutcomeIds', as.integer(n[i]))
    }
    return(y)
  } else {
    return('')
  }
}

#' @title getmaxdtslices 
#' @description Gets the max #dt slices in DBN (0=same time slice, 1 = t+1 etc.)
#' @param net - SMILE net object
#' @return node dt slices as int vector
#' @export
#' @examples none
#' @name getmaxdtslices
getmaxdtslices = function(net) {
  return( .jcall(obj=net, returnSig='I', method='getMaxTemporalOrder') + 1 ) # +1 as 0 is still one t slice (=BN)
}

#' @title getnodedtslices 
#' @description Gets the node dt slices (0=same time slice, 1 = t+1 etc.) 
#' @param net - SMILE net object
#' @param n - SMILE node id (integer)
#' @return node dt slices as int vector (sorted in ascending order)
#' @export
#' @examples none
#' @name getnodedtslices
getnodedtslices = function(net, n) {
  y = vector('integer',length=0)
  tryCatch({
    y = .jcall(obj=net, returnSig='[I',method='getTemporalOrders',as.integer(n))
    # Note this function does not return 0th time slice
  }, error = function(cond) {
    # Triggers -2 error if node is not a temporal node
  })
  return( sort(c(0,y)) ) 
}

#' @title getnodetemporaltype
#' @description Get the temporal type of a node. NOTE: ERROR IS THROWN IF NODE IS NOT-A-PLATE-NODE
#' @param net - SMILE net object
#' @param n - SMILE node id (integer)
#' @return integer, 0 = static, 3 = dynamic plate node
#' @export
#' @examples none
#' @name getnodetemporaltype
getnodetemporaltype = function(net,n) {
  nodetype=tryCatch({
    .jcall(obj=net,returnSig='I',method='getNodeTemporalType',as.integer(n))
  },error=function(cond){
    return(-1)
  })
  return(nodetype)
}

#' @title getparents 
#' @description Gets the node parents
#' @param net - SMILE net object
#' @param n - SMILE node id (integer)
#' @param dtslice - node dt slice (0 = same time slice, 1 = t+1 etc.)
#' @return list$parinds = node parent indices as vector of ints, $parnames = parent names as vector of chars
#' @export
#' @examples none
#' @name getparents
getparents = function(net, n, dtslice) {
  pn = vector('integer',0) # Parent node handles for non-temporal parents (integer)
  pndash = vector('integer',0) # Parent node handles for temporal parents (integer)
  pnorder = vector('integer',0) # Temporal parent order (i.e. t-j where j is 1, 2, 3...) (integer)
  # --- 1. Get parent node handles
  tryCatch({
    pn = .jcall(obj=net, returnSig='[I', method='getParents',as.integer(n)) # Get par node handle (int)
  },error = function(cond) {
    # Throws error if there are no parents
  }) # NOTE: if there is a t=0 arc, this is replicated AT ALL time slices - hence these parents are ALWAYS present
  tryCatch({
    pntemp = .jcall(obj=net, returnSig='[Lsmile/TemporalInfo;', method='getTemporalParents',
                    as.integer(n),as.integer(dtslice)) # pntemp is of class smile/TemporalInfo
    for(i in 1:length(pntemp)) { # extract int node handle from pntemp
      pndash[i] = pntemp[[i]]$handle # Note $handle is int node handle, $id is node name
      pnorder[i] = pntemp[[i]]$order
      # (but butchered sometimes), $order is 1 for t-1, 2 for t-2 etc.
    }
  },error = function(cond) {
    # Throws error if there are no temporal parents
  }) # end tryCatch
  
  # --- 2. Prepare outputs: parnames, parinds
  parinds = c(pn,pndash)
  parnames = vector('character',length(pn)+length(pndash))
  if(length(pn)>0) { # Add parent node names from non-temporal parents
    for(i in 1:length(pn)) {
      parnames[i] = getnodename(net,pn[i])
    }
  }
  if(length(pndash)>0) { # Add parent node names from temporal parents
    for(i in 1:length(pndash)) {
      parnames[i + length(pn)] = paste(.jcall(obj=net, returnSig='S', method='getNodeName',pndash[i]),
                                       sprintf('[t-%d]',pnorder[i]) )
    }
  }
  
  # --- 3. Return results
  return(list('parinds' = parinds, 'parnames' = parnames) )
}

#' @title getcpt 
#' @description Gets the node cpt table (no labels)
#' @param net - SMILE net object
#' @param n - SMILE node id (integer)
#' @param dtslice - node dt slice (0 = same time slice, 1 = t+1 etc.)
#' @return node cpt as a numeric matrix
#' @export
#' @examples none
#' @name getcpt
getcpt = function(net, n, dtslice) {
  if(dtslice==0) {
    cpt = .jcall(obj=net, returnSig='[D', method='getNodeDefinition',as.integer(n))
  } else {
    cpt = .jcall(obj=net, returnSig='[D', method='getNodeTemporalDefinition',
                 as.integer(n),as.integer(dtslice))
  }
  return(cpt)
}

#' @title getposterior
#' @description Get posterior probabilities for specified nodes and adds results to the end
#' of a posterior data frame (NOTE: does NOT check for duplicates).
#' @param net - SMILE DBN object
#' @param nodes - SMILE node id name (char or vector of chars)
#' @param nodestates - vector of chars of state labels OR list of vector of chars of state labels
#' @param chartime - vector of chars of datetime label - ASSUMES same chartime forall nodes; vector
#' length MUST be same length as numtslices (see setnumtslices)
#' @param scenario - the scenario ID (int) ASSUMES all nodes are for same scenario
#' @param posterior - data frame formatted as per dbn$posterior (see DBNSMILER-package 
#' documentation); $node,$state,$t,$p,$scenario,$pctile,$xbar
#' @return posterior data frame (see DBNSMILER-package documentation); if error, original
#' posterior data frame is returned, otherwise posteriors are ADDED to original posterior df
#' @export
#' @examples none
#' @name getposterior
getposterior = function(net,nodes,nodestates,chartime,scenario,posterior) {
  if(length(nodes)==1) { # only 1 node, convert nodestates, chartime to list
    nodestates = list(nodestates)
  }
  
  for(i in 1:length(nodes)) {
    p = NULL
    ind = nrow(posterior) + 1  # Row in posterior to start writing to
    success = tryCatch({
      p=.jcall(obj=net,returnSig='[D','getNodeValue',nodes[i]) # Get posterior pr
      1 # return 1 for success
    }, error=function(cond){
      print(cond)
      return(-1)
    })
    if(success==-1) {
      cat('Error obtaining posterior at node', nodes[i])
      return(posterior) # Return posterior if getting posterior for any node fails
    } 
    
    # Add new posteriors to posterior dataframe
    indrange = ind:(ind+length(chartime)*length(nodestates[[i]])-1)
    posterior[indrange,'node'] = nodes[i]
    posterior[indrange,'state'] = rep(nodestates[[i]],length.out=length(indrange))
    posterior[indrange,'t'] = rep(chartime, each=length(nodestates[[i]]))
    posterior[indrange,'p'] = rep(p,length.out=length(indrange))
    posterior[indrange,'scenario'] = scenario
    
#     # Check if posterior is read correctly - yes it is
#     if(nodes[i] %in% 'Realised_Shoot_Density') {
#       cat('^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n')
#       cat('Realised Shoot Density straight from SMILE\n')
#       pp <<- p
#       tt <<- rep(chartime, each=length(nodestates[[i]]))
#       ss <<- rep(nodestates[[i]],length.out=length(indrange))
# #       print(p)
#       print(identical(p,rep(p,length.out=length(indrange))))
#     }
  }
  return(posterior)
}

#' @title setnumtslices
#' @description Set the number of time slices for the DBN
#' @param net - SMILe net object
#' @param numslices - the number of time slices to set (this value needs to be big enough to cover
#' global date range)
#' @return none
#' @export
#' @examples none
#' @name setnumtslices
setnumtslices = function(net, numslices) {
  .jcall(obj=net, returnSig='V','setSliceCount', as.integer(numslices)) # Set #time slices
}

#' @title setinftype
#' @description Set the type of inference to perform. NOTE SMILE does not throw error if inftype
#' is invalid
#' @param net - SMILE DBN object
#' @param inftype - integer of the inference type, 3 is likelihood sampling 8 is EPIS algorithm
#' @return 1 if success, -1 if fail
#' @export
#' @examples none
#' @name setinftype
setinftype = function(net,inftype) {
  success=tryCatch({
    .jcall(obj=net,returnSig='V','setBayesianAlgorithm',as.integer(inftype)) 
    1
  },warning=function(cond){return(-1)},
  error=function(cond){return(-1)})
  return(success)
}

#' @title runinf
#' @description Run inference on DBN
#' @param net - SMILE DBN object
#' @return 1 if success, -1 if fail
#' @export
#' @examples none
#' @name runinf
runinf = function(net) {
  success=tryCatch({
    .jcall(obj=net,returnSig='V','updateBeliefs') # Update beliefs
    1
  },warning=function(cond){
    cat('Inference warning:',cond)
    return(-1)
  },
  error=function(cond){
    cat('Inference error:',cond)
    return(-1)})
  return(success)
}

#' @title setnumtslicesfordbn
#' @description Set the number of time slices for the DBN from the DBN global date range
#' @param dbn - DBNSMILER object (see definition in DBNSMILER-package)
#' @return 1 if success, -1 if fail
#' @export
#' @examples none
#' @name setnumtslicesfordbn
setnumtslicesfordbn = function(dbn) {
  # -- Work out time slices from global date range
  numslices = dbn$globdater$num[2] - dbn$globdater$num[1] + 1
  e=tryCatch({
    setnumtslices(dbn$net,numslices)
    1
  },error=function(cond){return(-1)})
  return(e)
}

#' @title chardate2num
#' @description This function converts a R char date string into numeric time in terms of 
#' #timeunits since 1900/1/1, where timeunits are: years, months, days, hours, minutes, seconds
#' @param strdate is a CHARACTER string of a POSIXlt date; can handle vector of strdates
#' @param timeunit - 'year','month','day','hour',minute','second'
#' @return numeric date - number of timeunits since 1900/1/1
#' @export
#' @examples none
#' @name chardate2num
chardate2num = function(strdate,timeunit) { 
  stint = as.POSIXlt(strdate)
  t = vector('numeric',length(strdate))
  # Calculate days since 1900-01-01
  d = as.numeric(as.Date(stint) - rep(as.Date('1900-01-01'),length.out=length(stint)) )
  for(i in 1:length(strdate)) {
    if(timeunit=='year') {
      t[i] = stint[i][['year']]
    } else if(timeunit=='month') {
      t[i] = stint[i][['year']]*12 + stint[i][['mon']]
    } else if(timeunit=='day') {
      t[i] = d[i]
    } else if(timeunit=='hour') {
      t[i] = d[i]*24 + stint[i][['hour']]
    } else if(timeunit=='minute') {
      t[i] = d[i]*24*60 + stint[i][['hour']]*60 + stint[i][['min']]
    } else if(timeunit=='second') {
      t[i] = d[i]*24*3600 + stint[i][['hour']]*3600 + stint[i][['min']]*60 + stint[i][['sec']]
    }
  }
  return(t)
}

#' @title num2chardate
#' @description Inverse of chardate2num - converts a numeric number of timeunits since 1900/1/1
#' to string. Timeunits are: years, months, days, hours, minutes, seconds
#' @param num is the number of timeunits since 1900/1/1; can be a vector
#' @param timeunit - 'year','month','day','hour',minute','second'
#' @return string date - %y, %y-%m, %y-%m-%d, %y-%m-%d-H, %y-%m-%d-%H-%M, %y-%m-%d-%H-%M-%S
#' format for 'year','month','day','hour',minute','second' timeunit respectively
#' @export
#' @examples none
#' @name num2chardate
num2chardate = function(num,timeunit) {
  str = vector('character',length(num))
  for(i in 1:length(num)) {
    n = num[i]
    if(timeunit=='year') {
      str[i] = paste(as.character(n+1900),'-1-1',sep='')
    } else if(timeunit=='month') {
      str[i] = paste(as.character(floor(n/12)+1900),'-',as.character(n%%12+1),'-1',sep='')
    } else if(timeunit=='day') {
      d = as.Date('1900-01-01') + n
      str[i] = format.Date(d,'%Y-%m-%d')
    } else if(timeunit=='hour') {
      d = as.Date('1900-01-01') + floor(n/24)
      str[i] = paste(format.Date(d,'%Y-%m-%d'),' ',as.character(n%%24),':00',sep='')
    } else if(timeunit=='minute') {
      d = as.Date('1900-01-01') + floor(n/24/60)
      M = n-floor(n/24/60)*24*60
      str[i] = paste(format.Date(d,'%Y-%m-%d'),' ',sprintf('%02d',floor(M/60)),':',
                     sprintf('%02d',(M%%60)),sep='')
    } else if(timeunit=='second') {
      d = as.Date('1900-01-01') + floor(n/24/3600)
      s = n-floor(n/24/3600)*24*3600
      sh = s-floor(s/3600)*3600
      str[i] = paste(format.Date(d,'%Y-%m-%d'),' ',sprintf('%02d',floor(s/3600)),':',
                     sprintf('%02d',floor(sh/60)),':',sprintf('%02d',sh%%60),sep='')
    }
  }
  return(str)
}




