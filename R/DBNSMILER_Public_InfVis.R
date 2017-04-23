# --------------------------------------------------------------------------------------------
# DBN 'Inference and Visualisation' public methods in DBNSMILER
# --------------------------------------------------------------------------------------------

#' @title editevid
#' @description Perform operations on the evidence table:add a row, modifyrow, remove, mvup, mvdown.
#' Format of evid table: $EvidID (int), $Scenario (char), $Node (name as str), $DateStart (char), 
#' $DateEnd (char), $SelectState (char), SoftEvidence (char); see DBNSMILER-package documentation 
#' @param action - 'add', 'modify', 'remove', 'mvup', 'mvdown'
#' @param evid table (data frame)
#' @param evidrow - a vector of char corresponding to a row of the evid table to add/modify
#' @param sr - integer of the selected row id
#' @return evid table formatted/edited; returns input arg 'evid' if error check fails
#' @export
#' @examples none
#' @name editevid
editevid = function(action,evid,evidrow,sr) {
  evidout = evid
  if((action=='add' || action=='modify') && 
       ( (ncol(evid) != length(evidrow)) || any(is.na(evidrow)) )   ) {
    return(evid) # Check length of evidrow = #columns of evid and make sure no NA's in evidrow
  }
  
  if(action=='add') { # Add to the end of evid
#     evidout = rbind(evidout,c(nrow(evidout)+1,evidrow[2:length(evidrow)]))
    evidout[nrow(evid)+1,2:ncol(evid)] = evidrow[2:length(evidrow)]
    evidout[nrow(evidout),1] = nrow(evidout)
  } else if(action=='modify') { # Modify evid at the selected row
    if(sr>0) {
      evidout[sr,2:ncol(evid)] = evidrow[2:length(evidrow)]
      evidout[sr,1] = sr
    }
  } else if(action=='remove') {
    if(sr>0 && sr<=nrow(evid) && nrow(evid)>0) {
      evidout = evidout[-sr,]
      evidout[1:nrow(evidout),1] = 1:nrow(evidout) # re-populate row numbers
    }
  } else if(action=='mvup') {
    if(nrow(evid)>0 && sr>1) { # Swap up
      evidout[sr+0,2:ncol(evid)] = evid[sr-1,2:ncol(evid)]
      evidout[sr-1,2:ncol(evid)] = evid[sr+0,2:ncol(evid)]
    }
  } else if(action=='mvdown') {
    if(nrow(evid)>0 && sr<nrow(evid)) { # Swap down
      evidout[sr+0,2:ncol(evid)] = evid[sr+1,2:ncol(evid)]
      evidout[sr+1,2:ncol(evid)] = evid[sr+0,2:ncol(evid)]
    }
  } # end if statements
  rownames(evidout) = make.names(rownames(evidout), unique=TRUE) # Addresses inconsistent issue in R
  # where sometimes, unique row names for data frame are automatically added by R,but sometimes not; 
  return(evidout)
}

#' @title setevid
#' @description Takes as input the selected set/subset of evidence E for a scenario; E is assumed to
#' have the same struct as evid (see DBNSMILER-package documentation)  E$EvidID (int), 
#' $Scenario (char), $Node (name as str), $DateStart (char), $DateEnd (char), $SelectState (char),
#' SoftEvidence (char).
#' NOTE: given the same node, evidence in later rows (i.e. lower in the table) will write over
#' evidence in earlier rows. For example, to set evid for X to be X1 at all times except at
#' t2-t3 set it to X2, do: [row 1 - set X=X1][row 2 - set X=X2 at t2-t3]
#' @param evid table (data frame) 
#' @param net SMILE DBN object
#' @param globdater defines simulation window; $char 2 element of datetime, 
#' $num, 2-element of datenum
#' @param datetype 'year','month','day','hour','minute','second'
#' @return 1 for success, -1 for failure
#' @export
#' @examples none
#' @name setevid
setevid = function(E, net, globdater, datetype) {
  success = tryCatch({
    # Get each item of evidence, identify time slices to set evidence, select state vs soft
    for(j in 1:nrow(E)) {
      t = chardate2num(c(E$DateStart[j],E$DateEnd[j]), datetype)
      # Get node index from node name/id and nodetype 0=static, 3=plate/dynamic
      n = .jcall(obj=net,returnSig='I',method='getNode',E$Node[j]) 
      nodetype = .jcall(obj=net,returnSig='I',method='getNodeTemporalType',n) 
      
      # Intersect trange with globdater trange - 3 cases
      if(t[1]<globdater$num[1] && t[2]>=globdater$num[1] && t[2]<=globdater$num[2]) {
        trange = c(globdater$num[1],t[2])
      } else if(t[1]>=globdater$num[1] && t[1]<=globdater$num[2] &&
                  t[2]>=globdater$num[1] && t[2]<=globdater$num[2]) {
        trange = c(t[1],t[2])
      } else if(t[1]>=globdater$num[1] && t[1]<=globdater$num[2] && t[2]>globdater$num[2]) {
        trange = c(t[1],globdater$num[2])
      } else {
        trange = NA
      }
      
      if(!any(is.na(trange))) {
        # Convert trange to t-slice range (which goes from 1 to numslices)
        trangevect = (trange[1]-globdater$num[1]+1):(trange[2]-globdater$num[1]+1)
        
        for(k in 1:length(trangevect)) { # Set the evidence!
          if(E$SelectState[j] != 'NA') {
            # Select state
            if(nodetype==0) { # Static node
              .jcall(obj=net,returnSig='V',method='setEvidence',n,
                     E$SelectState[j])
            } else { # Dynamic (plate) node
              .jcall(obj=net,returnSig='V',method='setTemporalEvidence',
                     n,as.integer(trangevect[k]-1),E$SelectState[j])
            }
          } else {
            # Virtual evidence
            tryCatch({ # Convert char to numeric vector
              numvec = as.numeric(unlist(strsplit(E$SoftEvidence[j],split=','))) 
            }, warning=function(w){})
            if(nodetype==0) { # Static node
              .jcall(obj=net,returnSig='V',method='setVirtualEvidence',n,
                     numvec)
            } else { # Dynamic (plate) node
#               sprintf('Node: %s, t=%g, p=%g,%g\n',E$Node[j],trangevect[k]-1,numvec[1],numvec[2])
              .jcall(obj=net,returnSig='V',method='setTemporalEvidence',
                     n,as.integer(trangevect[k]-1),as.numeric(numvec))
            }
          } # end if evidence is selectstate or soft
        } # end foreach time slice
      } # end if in trange
    } # end for each evidence row
    1 # return success
  },error=function(cond){return(-1)})
  return(success)
}

#' @title gendatetimeevid
#' @description Generate evidence for setting a datetime node in the DBN (e.g. for a 
#' 'Time_of_Year' node and dbn$datetype=months, generate evid=Jan, Feb, Mar etc. foreach month)
#' @param evidence - dbn$evid structure (see def in DBNSMILER-package)
#' @param node - char name (id) of datetime node (e.g. 'Time_of_Year')
#' @param chartime - vector of chars of datetime label corresponding to time range for simulation
#' vector length SHOULD be same length as numtslices (see setnumtslices)
#' @param scenario - the scenario ID (char) 
#' @param states (vector of char state labels for the datetime node)
#' @param datetype ('month' only one currently supported)
#' @return evid with datetime node evidence added; returns evid if function fails
#' @export
#' @examples none
#' @name gendatetimeevid
gendatetimeevid = function(evidence,node,chartime,scenario,states,datetype) { 
  evid = as.data.frame(evidence)
  ind = nrow(evid)
  for(i in 1:length(chartime)) {
    evid[i+ind,] = evid[i,] # First add an extra row, then overwrite
    evid[i+ind,'EvidID'] = max(evid$EvidID[evid$EvidID<1e6]) + i + 1e6
    evid[i+ind,'Scenario'] = scenario
    evid[i+ind,'Node'] = node
    evid[i+ind,'DateStart'] = chartime[i]
    evid[i+ind,'DateEnd'] = chartime[i]
    evid[i+ind,'SoftEvidence'] = ''
    if(datetype=='month') {
      j = as.POSIXlt(chartime[i])$mon + 1
      evid[i+ind,'SelectState'] = states[j]
    } else {
      cat('Currently, only "month"datetype is supported\n')
      return(evid)
    }
#     if(tres=='season') {
#       if(x=='Dec' || x=='Jan' || x=='Feb') {
#         E[i,'SelectState'] = 'Summer'
#       } else if(x=='Mar' || x=='Apr' || x=='May') {
#         E[i,'SelectState'] = 'Autumn'
#       } else if(x=='Jun' || x=='Jul' || x=='Aug') {
#         E[i,'SelectState'] = 'Winter'
#       } else if(x=='Sep' || x=='Oct' || x=='Nov') {
#         E[i,'SelectState'] = 'Spring'
#       }
#     }
  }
  rownames(evid) = make.names(rownames(evid), unique=TRUE) # Addresses inconsistent issue in R
  # where sometimes, unique row names for data frame are automatically added by R,but sometimes not;
  return(evid)
} 

#' @title rundbn
#' @description Runs the DBN.
#' Takes as input the selected set/subset of evidence E for a scenario 
#' NOTE - re-setting time slice should not be necessary, but bug with SMILE causes change 
#' to number of slices to 3 after set evid, regardless of what numslices was
#' @param dbn DBNSMILER object (see DBNSMILER-package documentation)
#' @param evid table (data frame) assumed to have th
#' e same struct as dbn$evid (see DBNSMILER-package documentation)  E$EvidID (int), 
#' $Scenario (char), $Node (name as str), $DateStart (char), $DateEnd (char), $SelectState (char),
#' SoftEvidence (char)
#' @param inftype - integer of the inference type, 3 is likelihood sampling 8 is EPIS algorithm
#' @param dtnode - char name (id) of datetime node (e.g. 'Time_of_Year'); set to null if unused
#' @param datetype 'year','month','day','hour','minute','second'
#' @param rettype - 0 to return the dbn with dbn$posterior updated, 1 to just return posterior
#' @return dbn updated dbn object after inference and updated with results/marginals
#' Note, only the SMILE DBN (via dbn$net) and dbn$posterior are updated
#' @export
#' @examples none
#' @name rundbn
rundbn = function(dbn,evid,inftype,dtnode,datetype,rettype) {
  success = TRUE
  posterior = dbn$posterior[rep(FALSE,length.out=nrow(dbn$posterior)),] #initialise empty posterior
  # Create char time vector over simulation range globdater
  chartime = num2chardate(dbn$globdater$num[1]:dbn$globdater$num[2],dbn$datetype)
  uniqscns = unique(evid$Scenario) # Unique scenarios
  if(length(uniqscns)<1) {uniqscns = as.integer(1)} # If no evidence, run DBN anyway

  for(i in 1:length(uniqscns)) { # --- For each scenario
    dbn = renewdbn(dbn) # 1 Clear all evidence by re-initialising network
    if(nrow(evid)>0) {
      E = evid[evid$Scenario %in% uniqscns[i],]
      if(!is.null(dtnode)) {
        E = gendatetimeevid(E,dtnode,chartime,1,dbn$node[[findnodebyname(dbn,dtnode)]]$states,
                            datetype) # Generate and add datetime evidence
      }
      success = success & setevid(E,dbn$net,dbn$globdater,dbn$datetype) # Set evidence
    }
    setinftype(dbn$net,inftype) # Set type of inference
    success = success & setnumtslicesfordbn(dbn) # Set #time slices for simulation
    # NOTE - re-setting time slice should not be necessary, but bug with SMILE causes change
    # to number of slices to 3 after set evid, regardless of what numslices was
    if(success>0) {
      success = runinf(dbn$net) # Perform inference
    
      # Get posterior
      nodes = vector('character',length(dbn$node))
      nodestates = vector('list',length(dbn$node))
      for(j in 1:length(dbn$node)) {
        nodes[j] = dbn$node[[j]]$name
        nodestates[[j]] = dbn$node[[j]]$states
      }
      posterior=getposterior(dbn$net,nodes,nodestates,chartime,uniqscns[i],posterior)
    }
  } # end for each scenario
  cat('Success for inference is:',success,'\n')
  # If returntype rettype is missing or 0, return dbn; if 1, return posterior
  if(missing(rettype)) {
    dbn$posterior = posterior
    return(dbn)
  } else {
    if(rettype==0) {
      dbn$posterior = posterior
      return(dbn)
    } else if(rettype==1) {
      return(posterior)
    }
  }
}

#' @title fastwmean
#' @description Calculates the weighted mean from posterior probabilities assuming a uniform
#' distribution across states, at specified percentiles. The results are added to the 
#' posterior object. This fast version ASSUMES posterior data frame follows a consistent order
#' w.r.t. results - i.e. sorted by scenarios, then, nodes, then time, then node states as
#' put together by getposterior().
#' @param posterior - data frame formatted as per dbn$posterior (see DBNSMILER-package 
#' documentation); $node,$state,$t,$p,$scenario,$pctile,$xbar
#' @param dbn DBNSMILER object (see DBNSMILER-package documentation). Use dbn to match node 
#' names to node state thresholds & states needed to determine expected value assuming uniform
#' distributions.
#' @param pctiles- numeric vector of percentiles for evaluating expected value=weighted mean at
#' @return posterior data frame (see DBNSMILER-package documentation) with expected val/weighted
#' mean results added; if error, original posterior data frame is returned. Note that all existing
#' expected val/weighted mean results in posterior are untouched.
#' @export
#' @examples none
#' @name fastwmean
fastwmean = function(posterior,dbn,pctiles) {
  Pout = posterior
  if(nrow(Pout)<1) {
    return(Pout)
  }
  for(i in 1:length(dbn$node)) { # for each node
    nodename = dbn$node[[i]]$name
    states = dbn$node[[i]]$states
    ntnum = as.numeric( unlist(strsplit(dbn$node[[i]]$nodethresh,',')) )
    
    if(!any(is.na(ntnum)) && length(ntnum)>0) { # check if thresholds exist
      # Weighted mean calculated from threshlow, threshupp, p and pctile - do this using
      # 3D matrices of threshlow, threshupp and p where rows=times&scenarios, col=pctiles,
      # 3D=states
      P = posterior[posterior$node %in% nodename & !is.na(posterior$p),]
      threshlow = array(0,dim=c(nrow(P)/length(states),length(pctiles),length(states)))
      threshupp = threshlow # Initialise threshlow and threshupp and p where
      p = threshlow # row=times&scenarios, col=states, 3D=pctiles
      Pctiles = threshlow
      for(j in 1:length(pctiles)) {
        Pctiles[,j,] = pctiles[j] # Pctiles 3D matrix
      }
      statelow=NA
      stateupp=NA
      Ithresh = NA
      if(length(ntnum)==2*length(states)) { # General case - apply weighted mean
        statelow = ntnum[seq(1,length(ntnum),2)] # Get lower thresholds foreach node state
        stateupp = ntnum[seq(2,length(ntnum),2)] # Get upper thresholds foreach node state
      } else if(length(ntnum)==(2*length(states)+1) && ntnum[length(ntnum)-1]==0 && 
                    ntnum[length(ntnum)-2]==0) { # Indicator fcn for zero state weighted mean
        statelow = ntnum[seq(1,length(ntnum)-1,2)] # Get lower thresholds foreach node state
        stateupp = ntnum[seq(2,length(ntnum)-1,2)] # Get upper thresholds foreach node state
        Ithresh = ntnum[length(ntnum)] # Threshold for indicator function
      }
      for(j in 1:length(states)) { # foreach state
        threshlow[,,j] = statelow[j]
        threshupp[,,j] = stateupp[j]
        Psub = P[P$state %in% states[j],]
        p[,,j] = Psub$p
      }
      mu = rowSums(p*(threshlow + (threshupp-threshlow)*Pctiles), dims=2)
      pzero = p[,,length(states)] # ASSUME that the zero state is the last state
      dim(pzero) = c(nrow(p),ncol(p))
      if(nrow(Psub)>0) {
        for(j in 1:length(pctiles)) { # Store results to posterior data frame via Pout
          Pdash = Psub # Copy all the other entries (e.g. node name, scenario id)
          Pdash$state = ''
          Pdash$p = NA
          Pdash$pctile = pctiles[j]
          if(is.na(Ithresh)) {
            Pdash$xbar = mu[,j]
          } else {
            I_mueq0 = as.numeric(!(mu[,j] <= Ithresh))
            Pdash$xbar = I_mueq0 *( (1-pzero[,j])*mu[,j] )
          }
          Pout = rbind(Pout,Pdash)
        } # end foreach percentile
      } # end make sure Psub has >0 rows - might happen w. inference error
    } # end if there are thresholds
  } # end foreach node
  return(Pout)
}

#' @title dbnplot
#' @description Plot weighted mean or state beliefs or both for specified DBN nodes. Handles both 
#' the case where posterior is missing $node and $scenario (as part of batch plotting - see 
#' visbatchdbn) or the normal posterior with $node and $scenario
#' @param sellabel - either 1) name of node to plot, or 2) ddf key (node+scn)
#' @param posterior - data frame formatted as per dbn$posterior (see DBNSMILER-package 
#' documentation) WITH/WITHOUT $node and $scenario; -$node,$state,$t,$p,-$scenario,$pctile,$xbar 
#' @param plottype - 'statebeliefs' (state probability plots) OR 'wmean' (weighted mean) OR 'both'
#' @param dateformat - char specifying x-axis date format, e.g. \%m-\%Y for months-year
#' \%d for day, \%H for hour, \%M for minute, \%S for second. Currently only handles dates.
#' @param customiseplots - if 1, looks for function customiseplotswrapper to customise plot colours
#' @return a ggplot object
#' @export
#' @examples none
#' @name dbnplot
dbnplot = function(sellabel,posterior,plottype,dateformat,customiseplots) {
  out = NULL
  # Check if posterior contains node and scenario
  if('node' %in% colnames(posterior) && 'scenario' %in% colnames(posterior)) {
    # Yes - so sellabel is actually the node name
    z = posterior[posterior$node %in% sellabel,]
  } else {
    # No - so sellabel
    z = cbind(posterior,node = sellabel)
  }
  
  # Add plottype column based on if p is NA (wmean) or not (statebelief)
  colplottype = vector('character',length=nrow(z))
  colplottype[is.na(z$p)] = 'wmean'
  colplottype[!is.na(z$p)] = 'state_p'  
  z = cbind(z,plottype = colplottype)

  if(plottype=='both') {
    out = ggplot()+
      geom_line(data=subset(z,plottype=='wmean'),
                aes(as.Date(t),xbar,group=pctile,alpha=factor(pctile))) + 
      geom_line(data=subset(z,plottype=='state_p'),aes(as.Date(t),p,group=state,colour=state)) +
      facet_grid(plottype~node,scales="free_y") + theme_bw() + xlab('Date') + ylab('')
  } else if(plottype=='statebeliefs') {
    out = ggplot()+
      geom_line(data=subset(z,plottype=='state_p'),aes(as.Date(t),p,group=state,colour=state)) +
      facet_grid(plottype~node,scales="free_y") + theme_bw() + xlab('Date') + ylab('')
  } else if(plottype=='wmean') {
    out = ggplot()+
      geom_line(data=subset(z,plottype=='wmean'),
                aes(as.Date(t),xbar,group=pctile,colour=factor(pctile))) + 
      facet_grid(plottype~node,scales="free_y") + theme_bw() + xlab('Date') + ylab('')
  }
  
  if(customiseplots) {
    out = tryCatch({
      if(!exists('dp')) {
        dp = NULL
      }
      customiseplotswrapper(out,sellabel,dp)
    },error=function(cond) {
      print(cond)
      out
    })
  }
  
  return(out)
}

#' @title getcptmask
#' @description Get rule 'activation strength'/probability mass over time
#' @param rules table as a data frame - format of rules table is as described in 
#' 'initrules'/DBNSMILER
#' @param CPT table data frame (formatted using 'formatcpt' - see DBNSMILER)
#' @param pnames vector of parent node names (char)
#' @param states vector of node state labels (char)
#' @return mask identifying which columns in CPT correspond to rule i; mask is list of length = 
#' unique(rules) and each element is vector mask of length ncol(cpt)-1
#' @export
#' @examples none
#' @name getcptmask
getcptmask = function(rules, tblcpt, pnames, states) {
  if(is.null(rules) || nrow(rules)<=2) {
    return(tblcpt) # No rules, return current CPT
  } 
  tblrules = rules[c(-1,-2),] # Remove the two header rows of tblrules
  
  # Sort tblrules
  tblrules = tblrules[order(tblrules[,length(pnames)+length(states)+2]),]
  # By sorting by priority, we will update the cpt with all lowest priority rules first
  # followed by rules of higher priority
  
  # However, sorting stuffs up the order of rules. 
  origtbl = rules[c(-1,-2),]
  origtbl = cbind(origtbl,origorder=1:nrow(origtbl))
  origtbl = origtbl[order(origtbl[,length(pnames)+length(states)+2]),]
  origorder = origtbl$origorder
  
  if(length(pnames)>0) {
    ruleant = as.matrix(tblrules[1:nrow(tblrules),2:(2+length(pnames)-1)])#antecedents
  } else {
    ruleant = NULL
  }
  rulecon = as.matrix(tblrules[1:nrow(tblrules),
                               (length(pnames)+2):
                                 (length(pnames)+1+length(states))])#consequents
  if(!is.null(ruleant)) {
    rmask = vector('numeric',ncol(tblcpt)-1) # Stored mask - this stores vecmask*rule index (=i) below to obtain the columns 
    # in the CPT associated with each rule
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
      rmask[vecmask] = i
      #       # Here, vecmask masks all the columns that match the antecedent parent states
      #       pr = maplingprtopr(rulecon[i,])
      #       # Update the CPT cells
      #       if(!is.null(pr) && sum(vecmask)>=1) {
      #         for(j in 1:length(states)) {
      #           tblcpt[j + length(pnames),c(FALSE,vecmask)] = pr[j] # col 1 contains headers
      #         }
      #       }
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
  rmask2 = vector('numeric',length(rmask))
  for(i in 1:length(origorder)) {
    rmask2[rmask==i] = origorder[i]
  }
  return(rmask2)
}

#' @title getpmass
#' @description Get rule 'activation strength'/probability mass over time and plot
#' @param dbn is DBNSMILER object
#' @param P is posterior data frame (data frame: node=char,state=char,t=char,p=numeric,scenario=int,
#' pctile=numeric,xbar=numeric expected value for given percentile,ybar=observed numeric exp. val 
#' for given percentile, py = numeric observed probability) - see DBNSMILER.
#' ASSUMES that P is sorted in increasing time order
#' ASSUMES that order of parents in dbn$node$parnodenames = order of parents in $formattedcpt
#' @param ind is index to dbn$node[[ind]] for selected node
#' @param tind is index to node time slice - e.g. dbn$node[[ind]]rules[[tind]]
#' @param labelthresh - (0,1) - max pr threshold for adding annotation with rule id on plot
#' @param plotrange - 2-element date/time vector (same datetype as dbn) of form [startdate,enddate]
#' defining thresholds for plotting
#' @return obj$pmass is data frame with $t = datetime (same units as dbn object), $rule = rule id
#' $p = probability mass p, $node = nodename; obj$plotobj = ggplot plot object of pmass over time
#' @export
#' @examples none
#' @name getpmass
getpmass = function(dbn,P,ind,tind,labelthresh,plotrange) {
  obj = NULL # object to return
  pmass = data.frame(node=vector('character',0),rule=vector('character',0),
                     t=vector('character',0),p=vector('numeric',0),stringsAsFactors=FALSE)
  n = dbn$node[[ind]]
  r = n$rules[[tind]] # rules
  if(nrow(r)<3) { # No rules for selected node and tind, return (1st 2 rows are header rows)
    obj$pmass = pmass
    obj$plotobj = NULL
    return(obj)
  }
  fcpt = n$formattedcpt[[tind]] # formatted cpt
  par = n$parnodenames[[tind]] # parent node names
  parstates = n$parstates[[tind]] # parent node states
  tvect = unique(P$t) # Time vector from t slice 1 to end
cat('Parents:',par,'\n')
print(parstates)
  rmask = getcptmask(r, fcpt, par,n$states) # rmask = vector mapping to cols in CPT storing rule index
  
  uniqr = sort(unique(rmask)) # unique rules (indices to rmask)
#   matparst = list() # list of matrices of par state names, row=parent, col=CPT column
#   for(i in 1:length(uniqr)) {
#     ncols = sum(rmask==uniqr[i])
#     matparst[[i]] = vector('list',length=length(par))# by length(par) then ncols of cpt for this rule .........#matrix(nrow=length(par),ncol=ncols)
#     for(j in 1:length(matparst[[i]])) { # foreach parent
#       matparst[[i]][[j]][1:ncols] = fcpt[j,c(FALSE,rmask==uniqr[i])]
#     } # end foreach parent
#   } # end foreach rule
# print(par)
# print(fcpt)
  
  # Prep p = antecedent probability matrix; assumes ncol same across all parents (i.e. width of cpt)
  p = matrix(1,nrow=length(tvect), ncol=ncol(fcpt)-1) # 1st column are labels
  
  for(i in 1:length(par)) { # foreach parent
    parname = gsub('([A-za-z0-9_ ]+)*\\[.+', '\\1',par[i]) # Extract nodename (inc. whitespace)
    parname = gsub('^\\s+|\\s+$','',parname) # Trim any leading/trailing whitespace
    parname = gsub(' ','_',parname)
    tslice = gsub('.+\\[.+?([0-9]+)\\]?.+$', '\\1', par[i]) # Get par node t-slice (-1, -2 etc.)
    tslice = suppressWarnings(as.numeric(tslice))
    tslice[is.na(tslice)] = 0 # If at same t-slice, set as 0
    
    # Fill in p - map parent node states to P$p
    for(j in 1:length(parstates[[i]])) {
      # mask all the cpt cols with this par node & state
      pvect = P$p[P$node %in% parname & P$state %in% parstates[[i]][j]] # node state pr over t
#       print(pvect)
#       print(P[P$node %in% parname & P$state %in% parstates[[i]][j],1:8])
      # Adjust pvect by t-slice - so if tslice=1, shift by 1 element, if tslice=2, shift by 2 etc.
      pvect[(tslice+1):length(pvect)] = pvect[1:(length(pvect)-tslice)]
cat('tslice is',tslice,'\n')
# pvect[1:(length(pvect)-tslice)] = pvect[(tslice+1):length(pvect)]
      # Apply to p
      mask = fcpt[i,2:ncol(fcpt)] %in% parstates[[i]][j]
#       aaa<<-p
      p[1:nrow(p),mask] = p[1:nrow(p),mask] * matrix(rep(pvect,length.out=nrow(p)*sum(mask)),nrow=nrow(p))
#       bbb<<-(p[1:nrow(p),mask] * pvect)
#       ddd<<-mask
#       ccc<<-p
#       if(identical(p[,mask],aaa[,mask]*pvect) ) {
#         cat('good\n')
#       } else {
#         cat('bad\n')
#       }
    }
  }
print(p)  
# Check that p * fcpt gives the marginals reported by Genie
states = n$states
cpt = matrix(n$cpt[[tind]],nrow=length(states))
# cat('nrow/ncol p=',nrow(p),ncol(p),'\n')
# cat('nrow/ncol fcpt = ',nrow(fcpt),ncol(fcpt),'\n')
pmarg = matrix(NA,nrow=length(states),ncol=nrow(p))
  for(i in 1:nrow(p)) {
    pmarg[1:length(states),i] = 
      rowSums(cpt * matrix(rep(p[i,1:ncol(p)],length.out=nrow(cpt)*ncol(cpt)),nrow=nrow(cpt),ncol=ncol(cpt),byrow=TRUE))
  }
# Compare to marginals from SMILE
# Assumes in order of time then states, i.e. 1/1/2015 hi lo, 1/2/2015 hi lo etc.
smarg = matrix(P$p[P$node %in% n$name & !is.na(P$p)],nrow=nrow(cpt))
# print(P[P$node %in% n$name & !is.na(P$p),])
cat('pmarg (calculated)')
print(pmarg[,1:25])
cat('========================================================================================\n')
cat('smarg (from SMILE)')
print(smarg[,1:25])
cat('========================================================================================\n')
print(abs(pmarg-smarg)[,1:25])
print(summary(t(abs(pmarg-smarg))))

  for(i in 1:length(uniqr)) { # foreach rule
    # Subset p by the columns included in that rule and sum across rows to get antecedent pr mass = 
    # degree of activation of that rule 
    irow = nrow(pmass)+1
    ncols = sum(rmask==uniqr[i])
    if(ncols>1) {
      pmass[irow:(irow+length(tvect)-1),] = cbind(n$name,uniqr[i],tvect,
                                                  rowSums(p[1:nrow(p),rmask==uniqr[i]]))
    } else if(ncols==1) {
      pmass[irow:(irow+length(tvect)-1),] =cbind(n$name,uniqr[i],tvect,p[1:nrow(p),rmask==uniqr[i]])
    } else { # else do nothing as this is rule is not represented in CPT at all
      cat('Warning - rule',uniqr[i],'is not represented in CPT - could be due to error or 
          overwritten by another rule\n')
    }
  }
  
  
  
  
#   for(i in 1:length(uniqr)) { # foreach rule
#     # Prep p matrix: assume ncol same across parents foreach rule
#     p = matrix(1,nrow=length(tvect), ncol=length(matparst[[i]][[1]]))
#     p2 = p
#     
#     for(j in 1:length(matparst[[i]])) { # foreach parent
#       parname = gsub('([A-za-z0-9_ ]+)*\\[.+', '\\1',par[j]) # Extract nodename (inc. whitespace)
#       parname = gsub('^\\s+|\\s+$','',parname) # Trim any leading/trailing whitespace
#       parname = gsub(' ','_',parname)
#       tslice = gsub('.+\\[.+?([0-9]+)\\]?.+$', '\\1', par[j]) # Get par node t-slice (-1, -2 etc.)
#       tslice = suppressWarnings(as.numeric(tslice))
#       tslice[is.na(tslice)] = 0 # If at same t-slice, set as 0
#       
#       # Fill in p - map parent node states to P$p
#       for(k in 1:length(parstates[[j]])) {
#         # mask all the cpt cols with this par node & state
#         pvect = P$p[grepl(parname,P$node) & P$state %in% parstates[[j]][k]] # node state pr over t
#         # Adjust pvect by t-slice - so if tslice=1, shift by 1 element, if tslice=2, shift by 2 etc.
#         pvect[(tslice+1):length(pvect)] = pvect[1:(length(pvect)-tslice)]
#         # Apply to p
#         mask = grepl(parstates[[j]][k],matparst[[i]][[j]])
#         p[,mask] = p[,mask] * pvect
#         
#         print(pvect)
#         print(mask)
#         stop()
#       }
      
      
      
      
#       for(k in 1:ncol(p)) { #foreach cpt col
#         cat('k=',k) # UP TO HERE
#         # Find p values for par node j state , assume to be sorted in ascending order of time
#         p2[,k] = P$p[grepl(parname,P$node) & P$state %in% matparst[[i]][[j]][k]]
#         # Adjust p2 by t-slice - so if tslice=1, shift by 1 element, if tslice=2, shift by 2 etc.
#         p2[(tslice+1):nrow(p2),k] = p2[1:(nrow(p2)-tslice),k] 
# #         print(tslice)
# #         print(par[j])
# #         print(parname)
# #         print(par[j]) # This contains whether parent is at t, t-1, t-2 etc.
# #         gsub('.+\\[.+?([0-9]+)\\]?.+$', '\\1','blah di blah [t - 32]')
# #         gsub('([A-za-z0-9 ]+)*\\[.+', '\\1', 'blah_di_blah [t - 357] xx')
# #         gsub('([[:punct:]])|\\s+','_','blah di blah')
#         # If we assume P is sorted in ascending order of time, then we could simply shift
#         # P by dt steps for [t-dt]; for indices <=dt, what do we do? Just use current I'd say
#         # p2[(dt+1):end,k] = p2[1:(end-dt),k] ok dokeys, test this out tomorrow!
#       }
#       p = p*p2
#     } 
#     irow = nrow(pmass)+1
#     pmass[irow:(irow+length(tvect)-1),] = cbind(n$name,uniqr[i],tvect,rowSums(p)) # Gives p of activation/pr mass foreach time step forthat rule
#   } # end foreach rule
  
  
  # ---------------------------------- PLOT RESULTS!
  pmass$t = as.Date(pmass$t)
  pmass$rule = factor(as.factor(pmass$rule),levels=as.character(uniqr))
  pmass$p = as.numeric(pmass$p)
  colours = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
              '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928',
              gray.colors(12, start = 0.1, end = 0.6, gamma = 2.2, alpha = NULL))
  colours = rep(colours,length.out=128)
  linetypes = rep(c('dotdash','longdash','solid','dotted','twodash'),length.out=128)

  
  # Limit plot to plotrange if not null
  if(!is.null(plotrange) || !is.na(plotrange)) {
    pmass2 = pmass[pmass$t>=plotrange[1] & pmass$t<=plotrange[2],]
  } else {
    pmass2 = pmass
  }
  plotobj = ggplot(pmass2,aes(x=t,y=p,group=rule,colour=rule,linetype=rule)) + geom_line(size=1)+
    scale_x_date(labels=date_format('%m-%Y')) + scale_colour_manual(values=colours)+
    scale_linetype_manual(values=linetypes)
  
  # Annotate the curves at max point where max >0.1
  for(i in 1:length(levels(pmass2$rule))) {
    x = pmass2[pmass2$rule %in% levels(pmass2$rule)[i],]
    if(max(x$p)>=labelthresh) {
      ind = which.max(x$p)
      plotobj = plotobj + annotate('text',x=x$t[ind],y=x$p[ind],label=as.character(x$rule[ind]))
    }
  }
  plotobj = plotobj + theme_bw()
  obj$pmass = pmass
  obj$plotobj = plotobj
  return(obj) 
}




































































#' @title calcxbar
#' @description Calculates the expected value from posterior probabilities assuming a uniform
#' distribution across states, at specified percentiles. The results are added to the 
#' posterior object. 
#' @param posterior - data frame formatted as per dbn$posterior (see DBNSMILER-package 
#' documentation); $node,$state,$t,$p,$scenario,$pctile,$xbar
#' @param dbn DBNSMILER object (see DBNSMILER-package documentation). Use dbn to match node 
#' names to node state thresholds & states needed to determine expected value assuming uniform
#' distributions.
#' @param pctiles- numeric vector of percentiles for evaluating expected value=weighted mean at
#' @return posterior data frame (see DBNSMILER-package documentation) with expected val/weighted
#' mean results added; if error, original posterior data frame is returned
#' @export
#' @examples none
#' @name calcxbar
calcxbar = function(posterior,dbn,pctiles) {
  Pout = posterior
  uniqnodes = unique(posterior$node) # Get unique nodes in data frame
  uniqscns = unique(posterior$scenario) # unique scenarios
  uniqtimes = unique(posterior$t) # unique time points
  uniqpctiles = unique(pctiles) # unique pctiles
  # For each combo of node, scenario, time, and percentile, calculate weighted mean=expect. val
  for(i in 1:length(uniqnodes)) {
    ind = findnodebyname(dbn,uniqnodes[i])
    states = dbn$node[[i]]$states
    ntnum = NA # node thresholds (numeric vector)
    tryCatch({
      ntnum = as.numeric( unlist(strsplit(dbn$node[[ind]]$nodethresh,',')) )
    },warning=function(cond){},error=function(cond){})
    
    if(length(ntnum)==2*length(states) && !any(is.na(ntnum))) {
      # General case - apply weighted mean
      statelow = ntnum[seq(1,length(ntnum),2)] # Get lower thresholds foreach node state
      stateupp = ntnum[seq(2,length(ntnum),2)] # Get upper thresholds foreach node state
      
      for(j in 1:length(uniqscns)) {
        for(k in 1:length(uniqtimes)) {
          P = posterior[posterior$node %in% uniqnodes[i] & posterior$scenario %in% uniqscns[j]
                        & posterior$t %in% uniqtimes[k],] # Subset the data
          Pout = rbind(Pout, wmean(P,states,statelow,stateupp,pctiles,NA))
        } # end for times
      } # end for scenarios
    } else if(length(ntnum)==2*length(states)+1 && !any(is.na(ntnum)) && 
                ntnum[length(ntnum)-1]==0 && ntnum[length(ntnum)-2]==0) {
      # Special case - apply weighted mean with indicator function for the zero state 
      # (i.e. last state has thresholds of 0,0,x where x is threshold for Indicator I_mueq0)
      statelow = ntnum[seq(1,length(ntnum)-1,2)] # Get lower thresholds foreach node state
      stateupp = ntnum[seq(2,length(ntnum)-1,2)] # Get upper thresholds foreach node state
      Ithresh = ntnum[length(ntnum)] # Threshold for indicator function
      
      for(j in 1:length(uniqscns)) {
        for(k in 1:length(uniqtimes)) {
          P = posterior[posterior$node %in% uniqnodes[i] & posterior$scenario %in% uniqscns[j]
                          & posterior$t %in% uniqtimes[k],] # Subset the data
          Pout = rbind(Pout, wmean(P,states,statelow,stateupp,pctiles,Ithresh))
        } # end for times
      } # end for scenarios
    } # end if
  } # End foreach node
  rownames(Pout) = make.names(rownames(Pout), unique=TRUE) # Addresses inconsistent issue in R
  # where sometimes, unique row names for data frame are automatically added by R,but sometimes not;
  return(Pout)
}


#' @title wmean
#' @description Calculates the weighted mean=expected value for the general case - see calcxbar
#' ASSUME order of states is same as that stored in SMILE and same as the order of thresholds
#' As part of this assumption, ASSUME that the last state is the Zero state if it exists.
#' @param P - subset of dbn$posterior (see DBNSMILER-package documentation) for a given:
#' node, scenario and time - therefore, nrow(P)=numstates of that node. P has
#' $node,$state,$t,$p,$scenario,$pctile,$xbar
#' @param states - node states as a vector of chars
#' @param statelow - vector of lower thresholds ASSUME same length as states and in same order
#' @param stateupp - vector of upper thresholds ASSUME same length as states and in same order
#' @param pctiles- numeric vector of percentiles for evaluating expected value=weighted mean at
#' @param Ithresh - threshold for zero indicator; set as NA if not in use
#' @return posterior data frame populated with weighted mean results
#' @export
#' @examples none
#' @name wmean
wmean = function(P,states,statelow,stateupp,pctiles,Ithresh) {
  Pout = P[rep(FALSE,length.out=nrow(P)),]
  indout = 1
  threshlow = numeric(nrow(P)) # Lower threshold foreach state
  threshupp = numeric(nrow(P)) # Upper threshold foreach state
  for(i in 1:nrow(P)) {
    ind = match(P$state[i],states) # First, find nodethresh for rows in P
    if(ind==length(states)) { # ASSUME that the zero state is the last state
      pzero = P$p[i]
    }
    threshlow[i] = statelow[ind]
    threshupp[i] = stateupp[ind]
  }
  threshdiff = threshupp - threshlow
  for(i in 1:length(pctiles)) {
    Pout[indout,] = P[1,] # Copy all the other columns: node,state,t,p,scenario,pctile,xbar
    Pout[indout,'state']=''
    Pout[indout,'p'] = NA
    Pout[indout,'pctile'] = pctiles[i]
    mu = sum(P$p * (threshlow + threshdiff * pctiles[i]))
    if(is.na(Ithresh)) {
      Pout[indout,'xbar'] = mu
    } else {
      I_mueq0 = 1
      if(mu <= Ithresh) { I_mueq0 = 0 }
      Pout[indout,'xbar'] = I_mueq0 *( (1-pzero)*mu )
    }
    indout = indout + 1
  }
  return(Pout)
}

#' @title clearevid
#' @description Clear all evidence in the SMILE object
#' DO NOT USE - SMILE THROWS EXCEPTIONS FOR CLEARTEMPORALEVIDENCE
#' @param dbn - a DBNSMILER object 
#' @return 1 if success, -1 if fail
#' @export
#' @examples none
#' @name clearevid
clearevid = function(dbn) {
  success = TRUE
  net = dbn$net
  setnumtslicesfordbn(dbn) # Set number of time slices
  for(i in 1:length(dbn$node)) {
    n = as.integer(dbn$node[[i]]$id)
    print(dbn$node[[i]]$name) # DEBUGGING
    nodetype = getnodetemporaltype(net,n)  # 0 for static nodes, 3 for dynamic nodes
    print(nodetype) # DEBUGGING
    e=tryCatch({
      if(nodetype==0) {
        .jcall(obj=net,returnSig='V',method='clearEvidence',n)
      } else if(nodetype==3) {
        numslices = .jcall(obj=net,returnSig='I',method='getSliceCount')
        for(j in 1:numslices) {
          .jcall(obj=net,returnSig='V','clearTemporalEvidence',as.integer(n),as.integer(j-1))
        }
      } # end if
      TRUE
    },error=function(cond){
      print(cond)
      return(FALSE)
    })
    success = success & e
  } # end for each node
  if(success) {
    return(1)
  } else {
    return(-1)
  }
}


############################################################# 
# TEMPORARY BACKUP
#############################################################
# #' @title getpmass
# #' @description Get rule 'activation strength'/probability mass over time and plot
# #' @param dbn is DBNSMILER object
# #' @param P is posterior data frame (data frame: node=char,state=char,t=char,p=numeric,scenario=int,
# #' pctile=numeric,xbar=numeric expected value for given percentile,ybar=observed numeric exp. val 
# #' for given percentile, py = numeric observed probability) - see DBNSMILER.
# #' ASSUMES that P is sorted in increasing time order
# #' @param ind is index to dbn$node[[ind]] for selected node
# #' @param tind is index to node time slice - e.g. dbn$node[[ind]]rules[[tind]]
# #' @param labelthresh - (0,1) - max pr threshold for adding annotation with rule id on plot
# #' @param plotrange - 2-element date/time vector (same datetype as dbn) of form [startdate,enddate]
# #' defining thresholds for plotting
# #' @return obj$pmass is data frame with $t = datetime (same units as dbn object), $rule = rule id
# #' $p = probability mass p, $node = nodename; obj$plotobj = ggplot plot object of pmass over time
# #' @export
# #' @examples none
# #' @name getpmass
# getpmass = function(dbn,P,ind,tind,labelthresh,plotrange) {
#   obj = NULL # object to return
#   pmass = data.frame(node=vector('character',0),rule=vector('character',0),
#                      t=vector('character',0),p=vector('numeric',0),stringsAsFactors=FALSE)
#   n = dbn$node[[ind]]
#   r = n$rules[[tind]] # rules
#   if(nrow(r)<3) { # No rules for selected node and tind, return (1st 2 rows are header rows)
#     obj$pmass = pmass
#     obj$plotobj = NULL
#     return(obj)
#   }
#   fcpt = n$formattedcpt[[tind]] # formatted cpt
#   par = n$parnodenames[[tind]] # parent node names
#   parstates = n$parstates[[tind]] # parent node states
#   tvect = unique(P$t) # Time vector from t slice 1 to end
#   
#   rmask = getcptmask(r, fcpt, par,n$states) # rmask = vector mapping to cols in CPT storing rule index
#   
#   uniqr = sort(unique(rmask)) # unique rules (indices to rmask)
#   matparst = list() # list of matrices of par state names, row=parent, col=CPT column
#   for(i in 1:length(uniqr)) {
#     ncols = sum(rmask==uniqr[i])
#     matparst[[i]] = vector('list',length=length(par))# by length(par) then ncols of cpt for this rule .........#matrix(nrow=length(par),ncol=ncols)
#     for(j in 1:length(matparst[[i]])) { # foreach parent
#       matparst[[i]][[j]][1:ncols] = fcpt[j,c(FALSE,rmask==uniqr[i])]
#     } # end foreach parent
#   } # end foreach rule
#   
#   for(i in 1:length(uniqr)) { # foreach rule
#     # Prep p matrix: assume ncol same across parents foreach rule
#     p = matrix(1,nrow=length(tvect), ncol=length(matparst[[i]][[1]]))
#     p2 = p
#     
#     for(j in 1:length(matparst[[i]])) { # foreach parent
#       parname = gsub('([A-za-z0-9_ ]+)*\\[.+', '\\1',par[j]) # Extract nodename (inc. whitespace)
#       parname = gsub('^\\s+|\\s+$','',parname) # Trim any leading/trailing whitespace
#       parname = gsub(' ','_',parname)
#       tslice = gsub('.+\\[.+?([0-9]+)\\]?.+$', '\\1', par[j]) # Get par node t-slice (-1, -2 etc.)
#       tslice = suppressWarnings(as.numeric(tslice))
#       tslice[is.na(tslice)] = 0 # If at same t-slice, set as 0
#       
#       for(k in 1:ncol(p)) { #foreach cpt col
#         cat('k=',k) # UP TO HERE
#         # Find p values for par node j state , assume to be sorted in ascending order of time
#         p2[,k] = P$p[grepl(parname,P$node) & P$state %in% matparst[[i]][[j]][k]]
#         # Adjust p2 by t-slice - so if tslice=1, shift by 1 element, if tslice=2, shift by 2 etc.
#         p2[(tslice+1):nrow(p2),k] = p2[1:(nrow(p2)-tslice),k] 
#         #         print(tslice)
#         #         print(par[j])
#         #         print(parname)
#         #         print(par[j]) # This contains whether parent is at t, t-1, t-2 etc.
#         #         gsub('.+\\[.+?([0-9]+)\\]?.+$', '\\1','blah di blah [t - 32]')
#         #         gsub('([A-za-z0-9 ]+)*\\[.+', '\\1', 'blah_di_blah [t - 357] xx')
#         #         gsub('([[:punct:]])|\\s+','_','blah di blah')
#         # If we assume P is sorted in ascending order of time, then we could simply shift
#         # P by dt steps for [t-dt]; for indices <=dt, what do we do? Just use current I'd say
#         # p2[(dt+1):end,k] = p2[1:(end-dt),k] ok dokeys, test this out tomorrow!
#       }
#       p = p*p2
#     } 
#     irow = nrow(pmass)+1
#     pmass[irow:(irow+length(tvect)-1),] = cbind(n$name,uniqr[i],tvect,rowSums(p)) # Gives p of activation/pr mass foreach time step forthat rule
#   } # end foreach rule
#   
#   
#   # ---------------------------------- PLOT RESULTS!
#   pmass$t = as.Date(pmass$t)
#   pmass$rule = factor(as.factor(pmass$rule),levels=as.character(uniqr))
#   pmass$p = as.numeric(pmass$p)
#   colours = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
#               '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928',
#               gray.colors(12, start = 0.1, end = 0.6, gamma = 2.2, alpha = NULL))
#   colours = rep(colours,length.out=128)
#   linetypes = rep(c('dotdash','longdash','solid','dotted','twodash'),length.out=128)
#   
#   
#   # Limit plot to plotrange if not null
#   if(!is.null(plotrange) || !is.na(plotrange)) {
#     pmass2 = pmass[pmass$t>=plotrange[1] & pmass$t<=plotrange[2],]
#   } else {
#     pmass2 = pmass
#   }
#   plotobj = ggplot(pmass2,aes(x=t,y=p,group=rule,colour=rule,linetype=rule)) + geom_line(size=1)+
#     scale_x_date(labels=date_format('%m-%Y')) + scale_colour_manual(values=colours)+
#     scale_linetype_manual(values=linetypes)
#   
#   # Annotate the curves at max point where max >0.1
#   for(i in 1:length(levels(pmass2$rule))) {
#     x = pmass2[pmass2$rule %in% levels(pmass2$rule)[i],]
#     if(max(x$p)>=labelthresh) {
#       ind = which.max(x$p)
#       plotobj = plotobj + annotate('text',x=x$t[ind],y=x$p[ind],label=as.character(x$rule[ind]))
#     }
#   }
#   plotobj = plotobj + theme_bw()
#   obj$pmass = pmass
#   obj$plotobj = plotobj
#   return(obj) 
# }

# #' @title dbnplot
# #' @description Create ggplot objects from posterior results, facet wrapped by scenario.
# #' @param posterior - data frame formatted as per dbn$posterior (see DBNSMILER-package 
# #' documentation); $node,$state,$t,$p,$scenario,$pctile,$xbar 
# #' @param nodes - vector of char node names for nodes to plot
# #' @param plottype - 'statebeliefs' (state probability plots) OR 'wmean' (weighted mean)
# #' @param dateformat - char specifying x-axis date format, e.g. \%m-\%Y for months-year
# #' \%d for day, \%H for hour, \%M for minute, \%S for second
# #' @return list of ggplot objects of same length as nodes
# #' @export
# #' @examples none
# #' @name dbnplot
# dbnplot = function(posterior,nodes,plottype,dateformat) { 
#   outplots = vector('list',length(nodes)) # List of ggplot objects
#   for(i in 1:length(nodes)) {
#     if(plottype=='statebeliefs') {
#       P = posterior[posterior$node %in% nodes[i] & !is.na(posterior$p),]
#       if(nrow(P)>0) {
#         P = transform(P,scenario = factor(P$scenario,levels=unique(scenario)))
#         objplot = ggplot(P,aes(x=as.Date(t),y=p,group=state,colour=state)) + theme_bw()
#         objplot = objplot + 
#           theme(text=element_text(size=16),axis.text.x=element_text(size=14))
#         objplot = objplot + ggtitle(nodes[i])
#         objplot = objplot + scale_x_date(labels=date_format(dateformat))
#         objplot = objplot + facet_wrap(~scenario)
#         if(length(unique(P$t))!=1) {
#           objplot = objplot + geom_line(size=1)
#         } else {
#           objplot = objplot + geom_point(size=6)
#         }
#         outplots[[i]] = objplot
#       }
#     } else if(plottype=='wmean') {
#       P = posterior[posterior$node %in% nodes[i] & is.na(posterior$p),]
#       if(nrow(P)>0) {
#         P = transform(P,scenario = factor(P$scenario,levels=unique(scenario)))
#         objplot = ggplot(P,aes(x=as.Date(t),y=xbar))
#         objplot = objplot + theme_bw()
#         objplot = objplot + 
#           theme(text=element_text(size=16),axis.text.x=element_text(size=14))
#         objplot = objplot + ggtitle(nodes[i])
#         objplot = objplot + scale_x_date(labels=date_format(dateformat))
#         objplot = objplot + facet_wrap(~scenario)
#         if(length(unique(P$t))!=1) {
#           objplot = objplot + geom_line(aes(group=pctile,alpha=factor(pctile)),
#                                         color='blue',size=0.7)
#         } else {
#           objplot = objplot + geom_point(aes(group=pctile,alpha=factor(pctile)),
#                                          color='blue',size=6)
#         }
#         alphavec = (1-abs(unique(P$pctile)-0.5))^3
#         objplot = objplot + scale_alpha_manual(values=alphavec)
#         outplots[[i]] = objplot
#       }
#     } else { # Unrecognised type
#       cat('Unrecognised plot type\n')
#       return(NULL)
#     }
#   }
#   return(outplots)
# }





