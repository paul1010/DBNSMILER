# --------------------------------------------------------------------------------------------
# DBN 'Batch computation' public methods in DBNSMILER
# --------------------------------------------------------------------------------------------
#' @title setevidbypattern
#' @description Set evidence using an annual temporal pattern
#' @param E is DBNSMILER evid data frame that is being added to: 
#' EvidID (int), $Scenario (char), $Node (name as str), 
#' $DateStart (char), $DateEnd (char), $SelectState (char), SoftEvidence (char); 
#' see DBNSMILER-package documentation
#' @param scnid is the scenario id
#' @param nodename = node name
#' @param pattern.t = time of year to apply evidence (numeric)
#' @param pattern.y = evidence to set (as char) - same length as pattern.t
#' @param start/end = start/end time (char - same format as DBNSMILER$globdater$char)
#' @param timeunit = 'month' (currenly the only supported time unit)
#' @return evid table formatted/edited; returns input arg 'evid' if error check fails
#' @export
#' @examples none
#' @name setevidbypattern
setevidbypattern = function(E,scnid,nodename,start,end,pattern.t,pattern.y,timeunit) {
  if(timeunit=='month') {
    # Generate vector of months from start to end
    startnum = chardate2num(start,'month')
    endnum = chardate2num(end,'month')
    datevec = startnum:endnum
    datevec = num2chardate(datevec,'month') # We want to extract the time of year
    datevecp = as.POSIXlt(datevec)
    mon = datevecp$mon + 1 # mon is then a vector containing the months foreach pt in datevec
    for(i in 1:length(mon)) {
      j = match(mon[i],pattern.t)
      if(!is.na(j)) {
        # If pattern.y is a char of numbers separated by commas, then its soft evid.
        num = NA
        tryCatch({
          num = as.numeric(unlist(strsplit(pattern.y[j],split=',')))
        },warning=function(cond){})
        
        if(any(is.na(num))) {# This is a select state evidence 
          E = editevid('add',E,c(0,scnid,nodename,datevec[i],datevec[i],pattern.y[j],'NA'),NA)
        } else { # This is soft evidence
          E = editevid('add',E,c(0,scnid,nodename,datevec[i],datevec[i],'NA',pattern.y[j]),NA)
        }
      } # End if
    } # End foreach element in mon
    return(E)
  } else {
    cat('Only the \'month\' time unit is currently supported\n')
    stop()
  }
}


#' @title runbatchdbn
#' @description Run a batch of DBN simulations according to specified evidence data frame.
#' Note this function uses datadr library.
#' @param evid is DBNSMILER evid data frame for scenarios: EvidID (int), $Scenario (char), 
#' $Node (name as str), $DateStart (char), $DateEnd (char), $SelectState (char), SoftEvidence (char)
#' see DBNSMILER-package documentation
#' @param dbn DBNSMILER object (see DBNSMILER-package documentation). Use dbn to run inference.
#' @param evidpath - path to local folder for storing evidence distributed data frame
#' @param postpath - path to local folder for storing posterior distributed data frame
#' @param postpath2 - path to local folder for storing re-structured by scenario and node posterior
#' distributed data frame for visualisation
#' @param pctiles - percentiles to do weighted mean calculations with (e.g. c(0,0.25,0.5,0.75,1))
#' @param numclusters - number of clusters to use for processing (1=no parallel processing)
#' @return List containing ddfevid, ddfp, ddfpost: posterior as distributed data frame (ddf-local 
#' disk). ddfp is divided as per evid ddf (drkeys), ddfpost is divided by scenario and node.
#' ddfevid is ddf version of evid divided by drkeys=scenario
#' Note the base dbn$posterior data frame struct is: $node,$state,$t,$p,$scenario,$pctile,$xbar
#' (see DBNSMILER-package documentation). Returns NA for both if errors.
#' 
#' @export
#' @examples none
#' @name runbatchdbn
runbatchdbn = function(evid,dbn,evidpath,postpath,postpath2,pctiles,numclusters) {
  ddfp = NA
  ddfpost = NA
  ddfevid = NA
  # Open connections to evidence and posterior data files
  if(sum(grepl('ddf',class(evid)))==0) { # Not a ddf
    keys = unique(evid$Scenario)
  } else {
    keys = getKeys(evid)
  }
  nbins = ceiling(length(keys) / 40) # 40 files per subdirectory
  dkconne = localDiskConn(evidpath,reset=TRUE,autoYes=TRUE,nBins=nbins) # evidence local disk connection
  dkconnp = localDiskConn(postpath,reset=TRUE,autoYes=TRUE,nBins=nbins) # posterior
  dkconnp2 = localDiskConn(postpath2,reset=TRUE,autoYes=TRUE,nBins=nbins) # re-structured posterior for vis
  
  # Divide evid - if evid is a data frame
  # Manually #####################################################NOTE BUG WITH DIVIDE, FAILS FOR 
  # BIG DATA FRAME BUT WORKS ON SMALLER DATA FRAMES
  if(sum(grepl('ddf',class(evid)))==0) { # Not a ddf, make a ddf
    eviddata = vector('list',length(keys))
    for(i in 1:length(keys)) {
      eviddata[[i]] = kvPairs(list(keys[i],evid[evid$Scenario %in% keys[i],]))[[1]]
    }
    ddfevid = ddf(eviddata,update=TRUE)
    addData(dkconne,ddfevid,overwrite=TRUE)
    ddfevid = ddf(dkconne,update=TRUE)
  } else { # already a ddf
    ddfevid = evid
  }
  
  # Automatically
#   evid = cbind(evid,evid$Scenario)
#   names(evid)[ncol(evid)] = 'drkeys'
#   ddfevid = divide(ddf(evid), by=condDiv('drkeys'),output=dkconne,update=TRUE,overwrite=TRUE)
  
  # Define run DBN transform
  f = function(x) {
    y = rundbn(dbn,x,8,'Time_of_Year',dbn$datetype,1) # rundbn
    return(fastwmean(y,dbn,pctiles))
  }
  drf = addTransform(ddfevid,f)
  # Run with or without clusters - 2 parts: calculation (rundbn+fastwmean), and restructure (divide)
  # - we re-structure posterior ddfp so it is divided by scenario and node
  if(numclusters>1) { # Run in parallel
    cl = makeCluster(4,type='SOCK')
    ddfp = recombine(drf,combDdo,output=dkconnp,
                     control=localDiskControl(cluster=cl),#,map_buff_size_bytes=16384),
                     overwrite=TRUE,params=list(dbn=dbn,pctiles=pctiles),packages=c('DBNSMILER'))
    ddfpost = tryCatch({ # Now re-structure result by scenario and node
      divide(ddfp,by=c('scenario','node'),output=dkconnp2,control=localDiskControl(cluster=cl),
             update=TRUE,overwrite=TRUE)
    }, error = function(cond) {
      ddf(dkconnp2)
    })
    stopCluster(cl)
  } else { # Run single core
    ddfp = recombine(drf,combDdo,output=dkconnp,overwrite=TRUE)
    ddfpost = tryCatch({ # Now re-structure result by scenario and node
      divide(ddfp,by=c('scenario','node'),output=dkconnp2,update=TRUE,overwrite=TRUE)
    }, error = function(cond) {
      ddf(dkconnp2)
    })
  }
  return(list(ddfp=ddfp,ddfpost=ddfpost,ddfevid=ddfevid))
}


#' @title visbatchdbn
#' @description Visualise a batch of DBN simulations according to posterior from runbatchdbn.
#' Note this function uses datadr and trelliscope library.
#' @param ddfpost: posterior as distributed data frame (local disk), divided by: scenario and node,
#' formatted as per dbn$posterior (see DBNSMILER-package documentation):
#' $node,$state,$t,$p,$scenario,$pctile,$xbar
#' @param dbn DBNSMILER object (see DBNSMILER-package documentation). Use dbn to run inference.
#' @param vdbpath - path to local folder for storing trelliscope visualisation
#' @param plottype - 'wmean', 'statebeliefs','both'
#' @param dateformat - char specifying x-axis date format, e.g. \%m-\%Y for months-year
#' \%d for day, \%H for hour, \%M for minute, \%S for second
#' @param customiseplots - if 1, looks for function customiseplotswrapper to customise plot colours
#' @return vdbconn - the visualisation database connection
#' @export
#' @examples none
#' @name visbatchdbn
visbatchdbn = function(ddfpost,vdbpath,plottype,dateformat,customiseplots) {
  # Create visualisation database
  vdbconn = vdbConn(vdbpath,name='DBN Output Visualisation',autoYes=TRUE)
  
  # Create plot panels
  # NOTE: these functions are executed at run time when viewing the display, not when the functions 
  # are called. Therefore, dateformat will be out of scope by then; trelliscope will look in global
  # vars for the variable.
  dateformat<<-'dateformat'
  if(plottype=='wmean') {
    plotobj = function(y,x) {
#       a=dbnsimpplot(x,'wmean',dateformat)
#       return(list(key=y,value=a))
    }
  } else if(plottype=='statebeliefs') {
    plotobj = function(y,x) {
#       a=dbnsimpplot(x,'statebeliefs',dateformat)
#       return(list(key=y,value=a))
    }
  } else if(plottype=='both') { # Plot both wmean and statebeliefs in a facet grid
    plotobj = function(y,x) {
      z=x
      z = cbind(z,as.character(is.na(z$p))) # Add plottype column based on if p is NA or not
      names(z)[ncol(z)] = 'plottype'
      out = ggplot()+geom_line(data=subset(z,plottype=='TRUE'),aes(as.Date(t),xbar,group=pctile,
                                                                   alpha=factor(pctile)))+
        geom_line(data=subset(z,plottype=='FALSE'),aes(as.Date(t),p,group=state,colour=state))+
        facet_grid(plottype~.,scales="free_y") + theme_bw()
      if(customiseplots) {
        out = tryCatch({
          customiseplotswrapper(out,y,dp)
        },error=function(cond) {
          print(cond)
          out
        })
      }
      return(list(key=y,value=out))
    }
  } else {
    cat('Unsupported plottype\n')
    stop()
  }
  # Make the display
  makeDisplay(ddfpost,name='DBNVis',desc='DBN Visualisation via Trelliscope',
              panelFn = plotobj, conn=vdbconn, params=list(dateformat=dateformat),
              packages=c('DBNSMILER'))
  return(vdbconn)
}






