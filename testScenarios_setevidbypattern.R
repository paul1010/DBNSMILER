# Add to DBNSMILER_batch
# Currently sets evid by "pattern" as in annual pattern




# Set evidence where pattern is a data frame containing columns:
# E is DBNSMILER evid data structure, scnid is the scenario id, nodename = node name,
# pattern.t = time of year (numeric), pattern.y = evidence to set (char); 
# start = start time, end = end time, timeunit = 'month' (currently the only supported time unit)
# ASSUMES: start, end are in DBNSMILER char time units
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