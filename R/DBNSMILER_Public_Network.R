# --------------------------------------------------------------------------------------------
# DBN 'Network' public methods in DBNSMILER
# --------------------------------------------------------------------------------------------

#' @title newdbn PUBLIC Constructor
#' @description This function creates a DBNSMILER object from a SMILE xdsl file
#' @param xdslfilepath - the file name plus path for the xdsl file to use
#' @return dbn structure object
#' @export
#' @examples none
#' @name newdbn
newdbn = function(xdslfilepath) {
  if(nargs()!=1) { # Check #inputs
    cat('newdbn must be called with one argument (DBN filename with path)\n')
    return(-1)
  }
  
  # Load BN file and try to read the nodes and store into nodelist
  n = 0
  err = tryCatch({
    net = .jnew('smile/Network')
    .jcall(obj=net, returnSig='V', method='readFile', xdslfilepath)
    n = .jcall(obj=net, returnSig='I', method='getFirstNode')
  }, error = function(cond){
    cat('Error reading/initialising DBN file:',xdslfilepath,'\nError is:',cond$message)
    return(-1)
  })
  if(err==-1) {return(-1)}
  
  dbn = initdbn(net, n, xdslfilepath)
  return( dbn )
}

#' @title renewdbn 
#' @description This function re-initialises the SMILE net object (clears all evidence etc.) by 
#' reloading from the xdsl file stored in dbn object.
#' ASSUMPTION: ASSUMES NO CHANGE TO XDSL FILE SINCE CALL TO newdbn
#' @param dbn object to renew (see def in DBNSMILER package)
#' @return updated dbn object (see def in DBNSMILER package)
#' @export
#' @examples none
#' @name renewdbn
renewdbn = function(dbn) {
  # Load BN file and try to read the nodes and store into nodelist
#   n = 0
  err = tryCatch({
    net = .jnew('smile/Network')
    .jcall(obj=net, returnSig='V', method='readFile', dbn$xdslfilepath)
    1
#     n = .jcall(obj=net, returnSig='I', method='getFirstNode')
  }, error = function(cond){
    cat('Error reading/re-initialising DBN file:',dbn$xdslfilepath,'\nError is:',cond$message)
    return(-1)
  })
  if(err==-1) {return(-1)}
  dbn$net = net
  return( dbn )
}

#' @title restruct
#' @description This function restructures a DBN data object according to another
#' @param dbndata - a DBNSMILER object to merge (use the data in this object - $node$notes, 
#' thresh, rules; $datetype, $globdater, $evid, $pplot, $lineplots, $contourplots)
#' @param dbnstruct - a DBNSMILER object to merge (use the order of nodes in this object, and node 
#' structure - $node$name, $id, $states, $dtslices, $parnodenames, $parnodeinds, $parstates, $cpt)
#' @return dbn - a dbn object
#' @export
#' @examples none
#' @name restruct
restruct = function(dbndata,dbnstruct) {
  dbn = tryCatch({
    dbn = dbnstruct
    for(i in 1:length(dbn$node)) {
      ind = findnodebyname(dbndata, dbn$node[[i]]$name)
      if(ind != -1) { # node was found in dbndata
        dbn$node[[i]]$notes = dbndata$node[[ind]]$notes
        dbn$node[[i]]$nodethresh = dbndata$node[[ind]]$nodethresh
        
        for(j in 1:length(dbn$node[[i]]$dtslices)) {
          dtind = match(dbn$node[[i]]$dtslices[j], dbndata$node[[ind]]$dtslices)
          if(!is.na(dtind) && length(dbndata$node[[ind]]$rules)>0) {
            dbn$node[[i]]$rules[[j]] = restructrules(dbndata$node[[ind]]$rules[[dtind]],
                                                     dbn$node[[i]]$rules[[j]])
          }
        } # end foreach dt slice
      }
    } # end foreach node
    dbn$datetype = dbndata$datetype
    dbn$globdater = dbndata$globdater
    dbn$evid = dbndata$evid
    dbn$pplot = dbndata$pplot
    dbn$lineplots = dbndata$lineplots
    dbn$contourplots = dbndata$contourplots
    dbn
  }, error=function(cond){
    return(-1)
  })
  return(dbn)
}

#' @title restructrules
#' @description This function restructures a rule table (data frame) according to another. Format of
#' rules table is as described in 'initrules'
#' @param rulesdata - a rules table to merge (use the actual rules rows in this object)
#' @param rulesstruct - a rules table to merge (use the row headers and dimensions of this object)
#' @return rules - a rules table, -1 if restructrules fails
#' @export
#' @examples none
#' @name restructrules
restructrules = function(rulesdata,rulesstruct) {
  rules = rulesstruct
    if(!is.null(rulesdata) && !is.null(rulesstruct) && nrow(rulesdata)>2) {
      rowrange = 3:nrow(rulesdata)
      rowrangepad = (nrow(rulesstruct)+1):(nrow(rulesstruct)+1+length(rowrange)-1)
      if(ncol(rulesdata)>ncol(rulesstruct)) { # Need to trim rulesdata before binding
        rules = rbind(rulesstruct, rulesdata[rowrange,1:ncol(rulesstruct)])
      } else if(ncol(rulesdata)<ncol(rulesstruct)) { # Need to pad rulesdata before binding
        rulesstruct[rowrangepad,1:ncol(rulesdata)] = rulesdata[rowrange,]
      } else { # Bind directly
        rules = rbind(rulesstruct, rulesdata[rowrange,])
      }
      rules[3:nrow(rules),1] = 1:(nrow(rules)-2)
    }
  return(rules)
}


#' @title findnodebyname 
#' @description Finds the index to dbn for the specified node
#' @param dbn - DBNSMILER structure object
#' @param nodename - string array containing the SMILE node name
#' @return index to dbn, -1 if not found
#' @export
#' @examples none
#' @name findnodebyname
findnodebyname = function(dbn, nodename) {
  for(i in 1:length(dbn$node)) {
    if(dbn$node[[i]]$name == nodename) {
      return(i)
    }
  }
  return(-1)
}

#' @title formatcpt
#' @description Adds labels to the raw cpt matrix (obtained with getcpt)
#' @param parnames - vector of parent node names (char)
#' @param parstates - list of vectors of parent state names (char)
#' @param cpt - node cpt as numeric matrix
#' @param states - vector of node state labels (char)
#' @return node cpt formatted with labels for display purposes
#' @export
#' @examples none
#' @name formatcpt
formatcpt = function(parnames, parstates, cpt, states) {
  tblnum = matrix(cpt,nrow=length(states)) # Turn CPT into a matrix
  colone = matrix(c(parnames,states), nrow=length(states)+length(parnames)) # col 1 headers
  numps = vector() # number of states in each parent
  rowhead = matrix() # CPT par state header rows
  
  if(length(parstates)>0) { # only get rowhead if there are parent nodes/states
    for(i in 1:length(parstates)) {
      numps[i] = length(parstates[[i]])
    }
    
    if(length(numps)>1) {
      nrepvec = cumprod(c(1,numps[1:(length(numps)-1)]))#num times to replicate vector of statelabels
      nrepsta = rev(cumprod(c(1,rev(numps[2:length(numps)])))) # num times to replicate each state
    } else {
      nrepvec = 1
      nrepsta = 1
    }  
    rowhead = matrix(nrow=length(parstates),ncol=prod(numps))
    # So, nrepsta goes from large#reps to 1 whilst nrepvec goes from 1 to large#reps    
    for(i in 1:length(parstates)) {
      rowhead[i,] = matrix(rep(rep(parstates[[i]],rep(nrepsta[i],length(parstates[[i]]))),
                               nrepvec[i]),1,prod(numps) )
    }
  }
  
  # Put into one table
  nrows = length(parnames)+length(states)
  ncols = prod(numps) + 1
  tblcpt = matrix(nrow=nrows,ncol=ncols)
  tblcpt[1:nrows,1] = colone
  
  if(length(parnames)>0) {tblcpt[1:length(parnames),2:ncols] = rowhead}
  tblcpt[(length(parnames)+1):nrows,2:ncols] = tblnum
  tblcpt <<- as.data.frame(tblcpt,stringsAsFactors=FALSE)
  return(tblcpt)
}

#' @title initrules
#' @description Initialises the rules table headers for given node
#' @param nodename - name of node (char)
#' @param states - vector of node state labels (char)
#' @param parnames - vector of parent node names (char)
#' @param parstates - list of vectors of char parent state names
#' @return node rules table with headers added (first 2 rows). These are made up of: col1 = 
#' rule id# (int), cols foreach parent node (antecedents, char), cols foreach node state (char), and
#' 1 col for rule priority. 
#' @export
#' @examples none
#' @name initrules
initrules = function(nodename, states, parnames, parstates) {
  na = length(parnames)
  ns = length(states)
  if(na>0) { # Create header block (parent node name + par state name, node state names)
    M = matrix(nrow=2,ncol=na+ns+1+1) # Include first column for rule number, last col=priority
    M[1,2:(na+1)] = rep('Antecedent',na)
    M[1,(na+2):(na+ns+1)] = rep(nodename,ns)
    M[2,2:(na+1)] = parnames
    M[2,(na+2):(na+ns+1)] = states
    M[2,ncol(M)] = 'Priority'
  } else { # For cases where there are no parent nodes
    M = matrix(nrow=2,ncol=ns+1+1)
    M[1,2:(ns+1)] = rep(nodename,ns)
    M[2,2:(ns+1)] = states
    M[2,ncol(M)] = 'Priority'
  }
  tblrules = as.data.frame(M,stringsAsFactors=FALSE)
  return(tblrules)
}

#' @title editrules
#' @description Perform operations on the rules table: add a row, modify row, remove, mvup, mvdown.
#' Note that the first two rows of rules are header rows. Format of rules table is as described in 
#' 'initrules'
#' @param action - 'add', 'modify', 'remove', 'mvup', 'mvdown'
#' @param rules table (data frame)
#' @param rulesrow - a vector of char corresponding to a row of the rules table to add/modify
#' @param sr - integer of the selected row id
#' @return node rules table formatted/edited; returns input arg 'rules' if error check fails
#' @export
#' @examples none
#' @name editrules
editrules = function(action,rules,rulesrow,sr) {
  rulesout = rules
  if((action=='add' || action=='modify') && ncol(rules) != length(rulesrow)+1) {
    return(rules) # Check length of rulesrow = #columns of rules
  }
  
  if(action=='add') { # Add to the end of rules
    j = nrow(rulesout)+1
    for(i in 1:length(rulesrow)) {
      rulesout[j,i+1] = paste(rulesrow[[i]],collapse=',')
    }    
#     rulesout[nrow(rules)+1,2:ncol(rules)] = rulesrow[1:length(rulesrow)]
    rulesout[nrow(rulesout),1] = nrow(rulesout)-2
  } else if(action=='modify') { # Modify rules at the selected row
    if(sr>0) {
      for(i in 1:length(rulesrow)) {
        rulesout[sr+2,i+1] = paste(rulesrow[[i]],collapse=',')
      } 
#       rulesout[sr+2,2:ncol(rules)] = rulesrow[1:length(rulesrow)]
      rulesout[sr+2,1] = sr
    }
  } else if(action=='remove') {
    if(sr>0 && sr<=(nrow(rules)-2) && nrow(rules)>2) {
      rulesout = rulesout[-(sr+2),]
      rulesout[3:nrow(rulesout),1] = 1:(nrow(rulesout)-2) # re-populate row numbers
    }
  } else if(action=='mvup') {
    if(nrow(rules)>2 && sr>1) { # Swap up
      rulesout[sr+2,2:ncol(rules)] = rules[sr+1,2:ncol(rules)]
      rulesout[sr+1,2:ncol(rules)] = rules[sr+2,2:ncol(rules)]
    }
  } else if(action=='mvdown') {
    if(nrow(rules)>2 && sr<(nrow(rules)-2)) { # Swap down
      rulesout[sr+2,2:ncol(rules)] = rules[sr+3,2:ncol(rules)]
      rulesout[sr+3,2:ncol(rules)] = rules[sr+2,2:ncol(rules)]
    }
  } # end if statements
  return(rulesout)
}

#' @title maplingprtopr
#' @description Map node state belief probabilities, described as linguistic variables in rules, to 
#' probabilities. Normalises so probabilities sum to 1.
#' @param linguisticprs linguistic probabilities as a vector of characters
#' @return vector of probabilities foreach state. -1 if error occurs.
#' @export
#' @examples none
#' @name maplingprtopr
maplingprtopr = function(linguisticprs) {
  pr = rep(0,length(linguisticprs)) # output probabilities
  if(is.list(linguisticprs)) {linguisticprs = unlist(linguisticprs)} #convert to vector if necessary
  for(i in 1:length(linguisticprs)) {
    if(linguisticprs[i]=='Certainty=1') {
      pr[i] = 1
    } else if(linguisticprs[i]=='Extremely Likely=0.99') {
      pr[i] = 0.99
    } else if(linguisticprs[i]=='Very Likely=5/6') {
      pr[i] = 5/6
    } else if(linguisticprs[i]=='Likely=2/3') {
      pr[i] = 2/3
    } else if(linguisticprs[i]=='50/50') {
      pr[i] = 0.5
    } else if(linguisticprs[i]=='Unlikely=1/3') {
      pr[i] = 1/3
    } else if(linguisticprs[i]=='Very Unlikely=1/6') {
      pr[i] = 1/6
    } else if(linguisticprs[i]=='Extremely Unlikely=0.01') {
      pr[i] = 0.01
    } else if (linguisticprs[i]=='Impossible=0') {
      pr[i] = 0
    } else {
      #stop('Error in maplingprtopr, linguistic state description not found')
      return(-1)
    } # end select
  } # end for loop
  # Normalise
  pr = pr / sum(pr)
  return(pr)
}

#' @title maprtoc
#' @description Map rules to node CPT
#' @param rules table as a data frame - format of rules table is as described in 'initrules'
#' @param CPT table data frame (formatted using 'formatcpt')
#' @param pnames vector of parent node names (char)
#' @param states vector of node state labels (char)
#' @return formatted CPT updated with rules. If error occurs, returns original tblcpt
#' @export
#' @examples none
#' @name maprtoc
maprtoc = function(rules, tblcpt, pnames, states) {
  if(is.null(rules) || nrow(rules)<=2) {
    return(tblcpt) # No rules, return current CPT
  } 
  tblrules = rules[c(-1,-2),] # Remove the two header rows of tblrules
  
  # Sort tblrules
  tblrules = tblrules[order(tblrules[,length(pnames)+length(states)+2]),]
  # By sorting by priority, we will update the cpt with all lowest priority rules first
  # followed by rules of higher priority
  if(length(pnames)>0) {
    ruleant = as.matrix(tblrules[1:nrow(tblrules),2:(2+length(pnames)-1)])#antecedents
  } else {
    ruleant = NULL
  }
  rulecon = as.matrix(tblrules[1:nrow(tblrules),
                               (length(pnames)+2):
                                 (length(pnames)+1+length(states))])#consequents
  if(!is.null(ruleant)) {
    for(i in 1:nrow(ruleant)) { # for each rule
      # Match to columns in the CPT
      vecmask = rep(TRUE,length.out=ncol(tblcpt)-1) # col1 of cpt is parent name heading column
      for(j in 1:ncol(ruleant)) { # for each parent node/antecedent
        if(ruleant[i,j] != 'NA') {
          # As there can be multiple state selections in a rules cell, iterate over them
          svec = unlist(strsplit( ruleant[i,j], ',')) # turn multiple states into vector
          mask = rep(FALSE,length.out=ncol(tblcpt)-1)
          for(k in 1:length(svec)) {
            mask = mask | (svec[k] == tblcpt[j,2:ncol(tblcpt)])
          }
          vecmask = vecmask & mask
        }
      } # end for each parent node
      # Here, vecmask masks all the columns that match the antecedent parent states
      pr = maplingprtopr(rulecon[i,])
      # Update the CPT cells
      if(!is.null(pr) && sum(vecmask)>=1) {
        for(j in 1:length(states)) {
          tblcpt[j + length(pnames),c(FALSE,vecmask)] = pr[j] # col 1 contains headers
        }
      }
    } # end for each rule
  } else { # This is a node where there are no parents, populate states directly
    for(i in 1:nrow(rulecon)) {
      pr = maplingprtopr(rulecon[i,])
      # Update the CPT cells
      if(!is.null(pr)) {
        for(j in (length(pnames)+1):nrow(tblcpt)) {
          tblcpt[j,c(FALSE,TRUE)] = pr[j]
        }
      }
    }
  } # end if
  return(tblcpt)
}
















