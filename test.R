# Testing
# Note: this file depends on Seagrass_v9f1.Rdata and Seagrass_v9f in the Seagrass Model directory

drive = 'C'
source(paste(drive,':/Work/AIMS Collaboration/Models and Data/testsmile.R',sep=''))
drive = 'C'
x=load(paste(drive,':/Work/AIMS Collaboration/Models and Data/Seagrass Model/Seagrass_v9jx.RData',sep=''))
dbndata = get(x)

library(DBNSMILER)
# Initialise model file names
filename = 'Seagrass_v9j'
modelfolder = 'Seagrass Model\\'
tres = 'month'
# filename = 'Coral_v4'
# modelfolder = 'Coral Model\\'
# tres = 'season'

setwd(paste(drive,':\\Work\\AIMS Collaboration\\Models and Data\\DBNSMILER\\DBNSMILER\\',sep=''))
# filejavaobjpath =paste(drive,":\\Work\\AIMS Collaboration\\Models and Data\\Smile\\smile.jar",
#                        sep='')
modelpath = paste(drive,':\\Work\\AIMS Collaboration\\Models and Data\\',modelfolder,sep='')
filexdsl = file.path(modelpath,paste(filename,'.xdsl',sep=''))
filerdata = file.path(modelpath,paste(filename,'.RData',sep=''))
# .jinit(classpath=filejavaobjpath)

# --------------------------------------------------------------------------------------------------
et = data.frame('t'=numeric(0),'description'=character(0),stringsAsFactors=FALSE)
et[1,1:2] = c(proc.time()[[3]],'refnewdbn') # For timing
# TEST DBNSMILER NETWORK METHODS
# 1) Test construction of new DBN
dbn = newdbn(filexdsl) # 'b.xdsl')
net = dbn$net
et[2,1:2] = c(proc.time()[[3]],'newdbn') # For timing
# 1a Test various get functions related to DBN esp. w.r.t. cpt, rules and getting node attributes
# in DBN structure
ind = findnodebyname(dbn,'Loss_in_Shoot_Density')
j = 1
n = dbn$node[[ind]]
cpt = formatcpt(n$parnodenames[[j]],n$parstates[[j]],n$cpt[[j]], n$states)
rules = initrules(n$name, n$states, n$parnodenames[[j]], n$parstates[[j]])

ind = findnodebyname(dbn,'Net_Change_Shoot_Density')
j = 1
n = dbn$node[[ind]]
cpt = formatcpt(n$parnodenames[[j]],n$parstates[[j]],n$cpt[[j]], n$states)
rules = initrules(n$name, n$states, n$parnodenames[[j]], n$parstates[[j]])
j = 2
n = dbn$node[[ind]]
cpt = formatcpt(n$parnodenames[[j]],n$parstates[[j]],n$cpt[[j]], n$states)
rules = initrules(n$name, n$states, n$parnodenames[[j]], n$parstates[[j]])

ind = findnodebyname(dbn,'Sediment_Load')
j = 1
n = dbn$node[[ind]]
cpt = formatcpt(n$parnodenames[[j]],n$parstates[[j]],n$cpt[[j]], n$states)
rules = initrules(n$name, n$states, n$parnodenames[[j]], n$parstates[[j]])

et[3,1:2] = c(proc.time()[[3]],'refrestruct') # For timing
# 2) Test restructuring and rules
# 2a Test restructuring
dbn=restruct(dbndata,dbn)
et[4,1:2] = c(proc.time()[[3]],'restruct') # For timing
# 2b Test manipulation of rules
n = dbn$node[[14]]
r = n$rules[[1]]
r=editrules('add',r,NULL,NA) # test error condition
r=editrules('add',r,c(1,'High','Very Likely=5/6','Very Unlikely=1/6','1=Normal'),NA)
r=editrules('mvup',r,NA,1)
r=editrules('mvup',r,NA,2)
r=editrules('mvdown',r,NA,1)
r=editrules('mvdown',r,NA,2)
r=editrules('remove',r,NA,2)

r=editrules('add',r,c(1,'High','Very Likely=5/6','Very Unlikely=1/6','1=Normal'),NA)
maplingprtopr(r[3,3:4])
maplingprtopr(r[4,3:4])

r=editrules('modify',r,c(1,'NA','50/50','50/50','0=Low'),1)
j = 1
maprtoc(r,n$formattedcpt[[j]], n$parnodenames[[j]], n$states)

ind = findnodebyname(dbn,'Physiological_Status_of_Plants')
j = 1
n = dbn$node[[ind]]
x=maprtoc(n$rules[[j]], n$formattedcpt[[j]], n$parnodenames[[j]], n$states)
x[,1:20]

j = 2
n = dbn$node[[ind]]
x=maprtoc(n$rules[[j]], n$formattedcpt[[j]], n$parnodenames[[j]], n$states)
x[,1:20]

ind = findnodebyname(dbn,'Sediment_Load')
j = 1
n = dbn$node[[ind]]
maprtoc(n$rules[[j]], n$formattedcpt[[j]], n$parnodenames[[j]], n$states)
# --------------------------------------------------------------------------------------------------
# TEST DBNSMILER INFERENCE METHODS

# 1) Test evid editing
e = dbn$evid[1:5,]
e=editevid('add',e,NULL,NA) # test error condition
e=editevid('add',e,c(1,2,'Accumulated_Light','2014-08-15','2015-08-15','AboveSaturation','NA'),NA)
e=editevid('modify',e,c(1,0,'Accumulated_Light','2014-08-15','2015-08-15','AboveSaturation','NA'),2)
e=editevid('remove',e,NA,1)
e=editevid('mvup',e,NA,1)
e=editevid('mvup',e,NA,2)
e=editevid('mvdown',e,NA,5)
e=editevid('mvdown',e,NA,4)

# 2) Test chardate2num and num2chardate conversions
a='2014-09-13 13:55:21'  # b='2012-08-31 8:20:00'
b=NA
b[1] = chardate2num(a,'year')
b[2] = chardate2num(a,'month')
b[3] = chardate2num(a,'day')
b[4] = chardate2num(a,'hour')
b[5] = chardate2num(a,'minute')
b[6] = chardate2num(a,'second')
num2chardate(b[1],'year')
num2chardate(b[2],'month')
num2chardate(b[3],'day')
num2chardate(b[4],'hour')
num2chardate(b[5],'minute')
num2chardate(b[6],'second')

# 3) Test evid setting
et[5,1:2] = c(proc.time()[[3]],'refsetevid') # For timing
setnumtslicesfordbn(dbn)
dbn = renewdbn(dbn) # for clearing old evidence; clearevid() doesn't work
E = dbn$evid[dbn$evid$Scenario %in% 3,]
setevid(E,dbn$net,dbn$globdater, dbn$datetype)
et[6,1:2] = c(proc.time()[[3]],'setevid') # For timing

# 4) Test inference
et[7,1:2] = c(proc.time()[[3]],'refruninf') # For timing
setinftype(dbn$net,3)
runinf(dbn$net)
et[8,1:2] = c(proc.time()[[3]],'runinf') # For timing

# 5) Get posterior
et[9,1:2] = c(proc.time()[[3]],'refgetposterior') # For timing
nodes = character(0)
nodestates = list()
for(i in 1:length(dbn$node)) {
  nodes[i] = dbn$node[[i]]$name
  nodestates[[i]] = dbn$node[[i]]$states
}
chartime = num2chardate(dbn$globdater$num[1]:dbn$globdater$num[2],'month')
P=getposterior(dbn$net,nodes,nodestates,chartime,3,dbn$posterior)
et[10,1:2] = c(proc.time()[[3]],'getposterior') # For timing

# 6) Test rundbn
str = 'Time_of_Year'
E = gendatetimeevid(dbn$evid,str,chartime,1,dbn$node[[findnodebyname(dbn,str)]]$states,'month')
et[11,1:2] = c(proc.time()[[3]],'refrundbn') # For timing
dbn = rundbn(dbn,dbn$evid,3,'Time_of_Year',dbn$datetype)
P = dbn$posterior[dbn$posterior$scenario==2,]
# P=calcxbar(P,dbn,c(0,0.25,0.5,0.75,1))
# P = calcxbar(dbn$posterior,dbn,c(0,0.25,0.5,0.75,1))
et[12,1:2] = c(proc.time()[[3]],'rundbn') # For timing

# 7) Test post-processing: weighted mean
et[13,1:2] = c(proc.time()[[3]],'refwmean') # For timing
P = fastwmean(dbn$posterior,dbn,c(0,0.25,0.5,0.75,1))
dbn$posterior = P
Px = P[is.na(P$p),]
et[14,1:2] = c(proc.time()[[3]],'wmean') # For timing

# 8) Plotting!
source('customiseplot.R')
et[15,1:2] = c(proc.time()[[3]],'refplot') # For timing
plotnodes = c('Realised_Shoot_Density','Overall_Lateral_Growth','Physiological_Status_of_Plants')
objplot = dbnplot(dbn$posterior,plotnodes,'statebeliefs','%m-%Y')
objplot = customiseplot(objplot,plotnodes)
objplot = dbnplot(dbn$posterior,plotnodes,'wmean','%m-%Y')
et[16,1:2] = c(proc.time()[[3]],'plot') # For timing
####################################################### TIMING
et$t = as.numeric(et$t)
et$dt[2:nrow(et)] = diff(et$t)
# For comparison, at work computer:
# t description
# 1 26.30   refnewdbn
# 2 28.12      newdbn
# 3 28.14 refrestruct
# 4 28.17    restruct
# 5 29.31  refsetevid
# 6 29.60     setevid





