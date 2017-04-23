formatrow = function(id,scnid,node,start,end,selectstate,softevidence) {
  # Returns formatted row for input to editevid (see DBNSMILER documentation)
  if(selectstate=='NA') {
    rowout = c(0,scnid,node,start,end,'NA')
  }
  rowout = c()
}
evid = editevid('add',evid,c(0,scnid,'Sediment_Quality',dpscn$Start[n2],
                             dpscn$End[n2],sedqual$SelectState[l],'NA'),NA)