# Test Scenarios
# Note: this file depends on Seagrass_v9j1.Rdata and Seagrass_v9j in the Seagrass Model directory

# Step to overcome Java bug
drive = 'C'
source(paste(drive,':/Work/AIMS Collaboration/Models and Data/testsmile.R',sep=''))

# --------------------------------------------------------------------------------------------------
drive = 'C'
library(DBNSMILER)
# library(foreach)
# library(doSNOW)
# Load the stored dbn structure
x=load(paste(drive,':/Work/AIMS Collaboration/Models and Data/Seagrass Model/Seagrass_v9jx.RData',sep=''))
dbndata = get(x)

# Initialise model file names
filename = 'Seagrass_v9j'
modelfolder = 'Seagrass Model\\'

setwd(paste(drive,':\\Work\\AIMS Collaboration\\Models and Data\\DBNSMILER\\DBNSMILER\\',sep=''))
modelpath = paste(drive,':\\Work\\AIMS Collaboration\\Models and Data\\',modelfolder,sep='')
filexdsl = file.path(modelpath,paste(filename,'.xdsl',sep=''))
filerdata = file.path(modelpath,paste(filename,'.RData',sep=''))
basename = 'Seagrass_v9j'
basenmdir = file.path(modelpath,basename,sep='')
# --------------------------------------------------------------------------------------------------
# Create new DBN

# Prepare timing matrix
et = data.frame('t'=numeric(0),'description'=character(0),stringsAsFactors=FALSE)

# Load and prepare DBN object
et[nrow(et)+1,1:2] = c(proc.time()[[3]],'refnewdbn') # For timing
dbn = newdbn(filexdsl) # 'b.xdsl')
net = dbn$net
et[nrow(et)+1,1:2] = c(proc.time()[[3]],'newdbn') # For timing
et[nrow(et)+1,1:2] = c(proc.time()[[3]],'refrestruct') # For timing
dbn=restruct(dbndata,dbn)
et[nrow(et)+1,1:2] = c(proc.time()[[3]],'restruct') # For timing

# --------------------------------------------------------------------------------------------------
# Populate global parameters
dbn$globdater$char[1] = '2015-01-01'
dbn$globdater$char[2] = '2021-12-31'
dbn$globdater$num = chardate2num(dbn$globdater$char,'month')

# --------------------------------------------------------------------------------------------------
# Prepare parameters for scenarios - dredge periods
et[nrow(et)+1,1:2] = c(proc.time()[[3]],'refprepparam') # For timing
# Dredge periods data frame = dp
dp =data.frame('tscncat'=character(0),'tscn'=character(0),'Duration'=numeric(0),'Dredge'=numeric(0),
               'StartMonth'=numeric(0),'Start'=character(0),'StartNum'=numeric(0),
               'End'=character(0),'EndNum'=numeric(0),stringsAsFactors=FALSE)
year = as.character(2017)
months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
tunit = 'month' # Time units

# 1 month period, evaluate monthly
irange = 1:12
dp[irange,] = NA
dp$tscncat[irange] = '1month'
dp$tscn[irange] = paste('1month',months,sep='-')
dp$Duration[irange] = 1 # 1 month, evaluate monthly
dp$Dredge[irange] = 1
dp$StartMonth[irange] = 1:12
dp$Start[irange] = paste(year,dp$StartMonth[irange],'01',sep='-')
dp$StartNum[irange] = chardate2num(dp$Start[irange],tunit)
dp$EndNum[irange] = dp$StartNum[irange]+dp$Duration[irange]-1 # Because evid t is inclusive
dp$End[irange] = num2chardate(dp$EndNum[irange],tunit)

# 2 month period, evaluate monthly
irange = 1:12 + nrow(dp)
dp[irange,] = NA
dp$tscncat[irange] = '2month'
dp$tscn[irange] = paste('2month',months,sep='-')
dp$Duration[irange] = 2 # 2 month, evaluate monthly
dp$Dredge[irange] = 1
dp$StartMonth[irange] = 1:12
dp$Start[irange] = paste(year,dp$StartMonth[irange],'01',sep='-')
dp$StartNum[irange] = chardate2num(dp$Start[irange],tunit)
dp$EndNum[irange] = dp$StartNum[irange]+dp$Duration[irange]-1 # Because evid t is inclusive
dp$End[irange] = num2chardate(dp$EndNum[irange],tunit)

# Wet vs dry season
irange = (1:4) + nrow(dp)
dp[irange,] = NA
dp$tscncat[irange] = 'WetDry'
dp$tscn[irange] = paste('WetDry',months[c(2,5,8,11)],sep='-')
dp$Duration[irange] = 6 # 6 month, evaluate Feb, May, Aug, Nov
dp$Dredge[irange] = 1
dp$StartMonth[irange] = c(2,5,8,11)
dp$Start[irange] = paste(year,dp$StartMonth[irange],'01',sep='-')
dp$StartNum[irange] = chardate2num(dp$Start[irange],tunit)
dp$EndNum[irange] = dp$StartNum[irange]+dp$Duration[irange]-1 # Because evid t is inclusive
dp$End[irange] = num2chardate(dp$EndNum[irange],tunit)

# 12 month dredge
irange = (1:4) + nrow(dp)
dp[irange,] = NA
dp$tscncat[irange] = '12month'
dp$tscn[irange] = paste('12month',months[c(1,4,7,10)],sep='-')
dp$Duration[irange] = 12 # 12 month, evaluate Jan, Apr, Jul, Oct
dp$Dredge[irange] = 1
dp$StartMonth[irange] = c(1,4,7,10)
dp$Start[irange] = paste(year,dp$StartMonth[irange],'01',sep='-')
dp$StartNum[irange] = chardate2num(dp$Start[irange],tunit)
dp$EndNum[irange] = dp$StartNum[irange]+dp$Duration[irange]-1 # Because evid t is inclusive
dp$End[irange] = num2chardate(dp$EndNum[irange],tunit)

# Split - 6 months in one go
irange = (1:4) + nrow(dp)
dp[irange,] = NA
dp$tscncat[irange] = 'Split-6'
dp$tscn[irange] = paste('Split-6',months[c(1,4,7,10)],sep='-')
dp$Duration[irange] = 6 # 6 month, evaluate Jan, Apr, Jul, Oct
dp$Dredge[irange] = 1
dp$StartMonth[irange] = c(1,4,7,10)
dp$Start[irange] = paste(year,dp$StartMonth[irange],'01',sep='-')
dp$StartNum[irange] = chardate2num(dp$Start[irange],tunit)
dp$EndNum[irange] = dp$StartNum[irange]+dp$Duration[irange]-1 # Because evid t is inclusive
dp$End[irange] = num2chardate(dp$EndNum[irange],tunit)

# Split - 3 on, 3 off, 3 on 
irange = (1:8) + nrow(dp)
dp[irange,] = NA
dp$tscncat[irange] = 'Split-3'
dp$tscn[irange] = paste('Split-3',months[rep(c(1,4,7,10),each=2)],sep='-')
dp$Duration[irange] = rep(c(3,3),times=4) # 3 on, 3 off, 3 on, evaluate Jan, Apr, Jul, Oct
dp$Dredge[irange] = 1
irange1 = irange[c(1,3,5,7)]
dp$StartMonth[irange1] = c(1,4,7,10)
dp$Start[irange1] = paste(year,dp$StartMonth[irange1],'01',sep='-')
dp$StartNum[irange1] = chardate2num(dp$Start[irange1],tunit)
dp$EndNum[irange1] = dp$StartNum[irange1]+dp$Duration[irange1]-1 # Because evid t is inclusive
dp$End[irange1] = num2chardate(dp$EndNum[irange1],tunit)

irange2 = irange[c(2,4,6,8)]
dp$StartNum[irange2] = dp$StartNum[irange1] + 6
dp$Start[irange2] = num2chardate(dp$StartNum[irange2],tunit)
dp$EndNum[irange2] = dp$StartNum[irange2]+dp$Duration[irange2]-1 # Because evid t is inclusive
dp$End[irange2] = num2chardate(dp$EndNum[irange2],tunit)

# Split - 2 on, 2 off, 2 on, 2 off, 2 on
# Split - 3 on, 3 off, 3 on 
irange = (1:12) + nrow(dp)
dp[irange,] = NA
dp$tscncat[irange] = 'Split-2'
dp$tscn[irange] = paste('Split-2',months[rep(c(1,4,7,10),each=3)],sep='-')
dp$Duration[irange] = rep(c(2,2,2),times=4) # 2on,2off,2on,2off,2on evaluate Jan, Apr, Jul, Oct
dp$Dredge[irange] = 1
irange1 = irange[c(1,4,7,10)]
dp$StartMonth[irange1] = c(1,4,7,10)
dp$Start[irange1] = paste(year,dp$StartMonth[irange1],'01',sep='-')
dp$StartNum[irange1] = chardate2num(dp$Start[irange1],tunit)
dp$EndNum[irange1] = dp$StartNum[irange1]+dp$Duration[irange1]-1 # Because evid t is inclusive
dp$End[irange1] = num2chardate(dp$EndNum[irange1],tunit)

irange2 = irange[c(2,5,8,11)]
dp$StartNum[irange2] = dp$StartNum[irange1] + 4
dp$Start[irange2] = num2chardate(dp$StartNum[irange2],tunit)
dp$EndNum[irange2] = dp$StartNum[irange2]+dp$Duration[irange2]-1 # Because evid t is inclusive
dp$End[irange2] = num2chardate(dp$EndNum[irange2],tunit)

irange3 = irange[c(3,6,9,12)]
dp$StartNum[irange3] = dp$StartNum[irange2] + 4
dp$Start[irange3] = num2chardate(dp$StartNum[irange3],tunit)
dp$EndNum[irange3] = dp$StartNum[irange3]+dp$Duration[irange3]-1 # Because evid t is inclusive
dp$End[irange3] = num2chardate(dp$EndNum[irange3],tunit)

# --------------------------------------------------------------------------------------------------
# Prepare parameters for scenarios - node evidence parameters
ep = data.frame('Node'=character(0),'Dredge'=numeric(0),'SelectState'=character(0),
                    'SoftEvidence'=character(0),stringsAsFactors=FALSE)
# Assume Amphibolis = PersistentTemperateShallowSubTidal = Jurien Bay
ep[nrow(ep)+1,] = c('Genera_Presence',NA,'Amphibolis','NA')
# Assume Halophila = TransitoryTropicalDeepSubTidal = James Price Point
ep[nrow(ep)+1,] = c('Genera_Presence',NA,'Halophila','NA') 
ep[nrow(ep)+1,] = c('Location_Type',NA,'PersistentTemperateShallowSubTidal','NA')
ep[nrow(ep)+1,] = c('Location_Type',NA,'TransitoryTropicalDeepSubTidal','NA')
ep[nrow(ep)+1,] = c('Accumulated_Light',0,'AboveSaturation','NA')
ep[nrow(ep)+1,] = c('Accumulated_Light',1,'BelowSaturation','NA')
ep[nrow(ep)+1,] = c('Accumulated_Burial',0,'NoEffect','NA')
ep[nrow(ep)+1,] = c('Accumulated_Burial',1,'Effect','NA')
ep[nrow(ep)+1,] = c('Accumulated_Burial',1,'NA','0.5, 0.5')
ep[nrow(ep)+1,] = c('Sediment_Quality',0,'NoEffect','NA')
ep[nrow(ep)+1,] = c('Sediment_Quality',1,'Effect','NA')
ep[nrow(ep)+1,] = c('Sediment_Quality',1,'NA','0.5, 0.5')
ep[nrow(ep)+1,] = c('Physiological_Status_of_Plants',NA,'Good','NA')
ep[nrow(ep)+1,] = c('Physiological_Status_of_Plants',NA,'Poor','NA')
#initaerialextent=c('High','Medium','Low','Zero')#NOT DONE BECAUSE this does not feed into anything
et[nrow(et)+1,1:2] = c(proc.time()[[3]],'prepparam') # For timing

# --------------------------------------------------------------------------------------------------
# Deal with light patterns over the year
# Load setevidbypattern.R
source('testScenarios_setevidbypattern.R')
# Light patterns are obtained from data_explore.R in /Seagrass_Model/ folder
lamphibolis = c(rep('1,0',length.out=3),'0.98,0.02','0.89,0.11','0.77,0.23',
                rep('1,0',length.out=6))
# lamphibolis = c('AboveSaturation','AboveSaturation','0.89,0.11','0.77,0.23',
#                 rep('AboveSaturation',length.out=8))
# lamphibolis = rep('AboveSaturation',length.out=12)
ldamphibolis = c('0.14,0.86','0,1','0.03,0.97','0.01,0.99',rep('0,1',length.out=7),'0.05,0.95')
# ldamphibolis = rep('BelowSaturation',length.out=12)
# Amphibolis
# natural light is 1-12 c(0.999,0.99,0.89,0.77,0.998,0.998,0.998,0.998,0.998,0.998,0.998,0.998)
# Pretty much 1,2,5-12 are 1.0; only 3 has 0.89 and 4 has 0.77
# dredge light is 1-12 c(0.03,0.01,0.003,0.003,0.005,0.005,0.005,0.005,0.005,0.05,0.14,0.005)
# Pretty much dredge for 3-9,12 is 0; only 1 is 0.03, 2 is 0.01, 10 is 0.05, 11 is 0.14

lhalophila = c(rep('0.38,0.62',length.out=4),rep('0.77,0.23',length.out=6),
               rep('0.38,0.62',length.out=2))
# lhalophila = c(rep('1,0',length.out=12))
ldhalophila = rep('0,1',length.out=12)
# Halophila
# natural light is 1-12 c(0.38,0.38,0.38,0.38,0.77,0.77,0.77,0.77,0.77,0.77,0.38,0.38)
# Pretty much light is 1-4,11-12 is 0.38, 5-10 is 0.77
# dredge light is 1-12 c(0,0,0,0,0,0,0,0,0,0,0,0)
# Pretty much light is 1-12 is 0
timeunit = 'month'

# --------------------------------------------------------------------------------------------------
# Set light as: above sat. non-dredging, below sat. dredging
# 3 main batches of scenarios
# Batch A1: NoEffect burial or sedimentqual, uniqdp (1mth,2mth) by genera/loc 24*2 = 24*2
# Batch A2: NoEffect burial or sedimentqual, uniqdp (less 1mth,2mth) by genera/loc 20*2=20*2
# Batch B1: InitGoodPhysStatus uniqdp (12mth) x genera/loc x burial x sedimentqual 4*2*3*3= 36*2
# Batch B2: InitGoodPhysStatus uniqdp (Split-6) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch B3: InitGoodPhysStatus uniqdp (Split-3) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch B4: InitGoodPhysStatus uniqdp (Split-2) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch C1: initPoorPhysStatus uniqdp (12mth) x genera/loc x burial x sedimentqual 4*2*3*3= 36*2
# Batch C2: initPoorPhysStatus uniqdp (Split-6) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch C3: initPoorPhysStatus uniqdp (Split-3) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch C4: initPoorPhysStatus uniqdp (Split-2) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
evid.old = dbn$evid
dbn$evid = dbn$evid[-(1:nrow(dbn$evid)),]
globstart = dbn$globdater$char[1]
globend = dbn$globdater$char[2]
ebatch = vector('list',18)
# numgenera = sum(ep$Node %in% 'Genera_Presence',na.rm=TRUE)

et[nrow(et)+1,1:2] = c(proc.time()[[3]],'refgenevid') # For timing

# Batch A1-A2: NoEffect burial or sedimentqual, uniqdp (1mth,2mth) by genera/loc 24*2 = 24*2
dpsub = dp[dp$tscncat %in% c('1month','2month'),]
uniqscn = unique(dpsub$tscn)
ebatchind = c(1,2)
for(k in 1:2) {
  evid = dbn$evid
  for(i in 1:length(uniqscn)) { # foreach unique scenario
    scnid = dpsub$tscn[i]
    
    evid=editevid('add',evid,c(0,scnid,'Realised_Shoot_Density',globstart,globstart,
                               'High','NA'),NA)
    evid=editevid('add',evid,c(0,scnid,'Realised_Biomass',globstart,globstart,
                               'High','NA'),NA)
    if(k==1) {
      evid=editevid('add',evid,c(0,scnid,'Genera_Presence',globstart,globend,'Amphibolis','NA'),NA)
      evid = editevid('add',evid,c(0,scnid,'Location_Type',globstart,globend,
                                   'PersistentTemperateShallowSubTidal','NA'),NA)
    } else {
      evid=editevid('add',evid,c(0,scnid,'Genera_Presence',globstart,globend,'Halophila','NA'),NA)
      evid = editevid('add',evid,c(0,scnid,'Location_Type',globstart,globend,
                                   'TransitoryTropicalDeepSubTidal','NA'),NA)
    }
    evid = editevid('add',evid,c(0,scnid,'Accumulated_Burial',globstart,globend,'NoEffect','NA'),NA)
    evid = editevid('add',evid,c(0,scnid,'Sediment_Quality',globstart,globend,'NoEffect','NA'),NA)
#     evid = editevid('add',evid,c(0,scnid,'Accumulated_Light',globstart,globend,
#                                  'AboveSaturation','NA'),NA)
    if(k==1) { # Amphibolis
      evid = setevidbypattern(evid,scnid,'Accumulated_Light',globstart,globend,1:12,
                              lamphibolis,timeunit)
    } else { # Halophila
      evid = setevidbypattern(evid,scnid,'Accumulated_Light',globstart,globend,1:12,
                              lhalophila,timeunit)
    }
    evid = editevid('add',evid,c(0,scnid,'Physiological_Status_of_Plants',globstart,globstart,
                                 'Good','NA'),NA)
    # Add dredging effects on light only for Batch A1
    dpscn = dpsub[dpsub$tscn %in% uniqscn[i],]
    for(j in 1:nrow(dpscn)) { # foreach row in this scenario
      if(dpscn$Dredge[j]) {
#         evid = editevid('add',evid,c(0,scnid,'Accumulated_Light',dpscn$Start[j],dpscn$End[j],
#                                      'BelowSaturation','NA'),NA)
        if(k==1) { # Amphibolis
          evid = setevidbypattern(evid,scnid,'Accumulated_Light',dpscn$Start[j],dpscn$End[j],1:12,
                                  ldamphibolis,timeunit)
        } else { # Halophila
          evid = setevidbypattern(evid,scnid,'Accumulated_Light',dpscn$Start[j],dpscn$End[j],1:12,
                                  ldhalophila,timeunit)
        }
      }
    }
  }
  ebatch[[ebatchind[k]]] = evid
}

# Batch B1-B4 + C1-C4
# Batch B1: InitGoodPhysStatus uniqdp (12mth) x genera/loc x burial x sedimentqual 4*2*3*3= 36*2
# Batch B2: InitGoodPhysStatus uniqdp (Split-6) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch B3: InitGoodPhysStatus uniqdp (Split-3) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch B4: InitGoodPhysStatus uniqdp (Split-2) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch C1: initPoorPhysStatus uniqdp (12mth) x genera/loc x burial x sedimentqual 4*2*3*3= 36*2
# Batch C2: initPoorPhysStatus uniqdp (Split-6) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch C3: initPoorPhysStatus uniqdp (Split-3) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Batch C4: initPoorPhysStatus uniqdp (Split-2) x genera/loc x burial x sedimentqual 4*2*3*3 = 36*2
# Loop order: physstatus, batchdp, genera/loc, sedqual, burial,dp #######################
# NOTE: ASSUME GENERA AND LOCATIONTYPE ARE MATCHED, SO GENERA[i] AND LOC[i] TOGETHER FORM EVID
# FOR ONE PARTICULAR SCENARIO
physstatus = ep[ep$Node %in% 'Physiological_Status_of_Plants',]
batchdp = list()
batchdp[[1]] = dp[dp$tscncat %in% '12month',]
batchdp[[2]] = dp[dp$tscncat %in% 'Split-6',]
batchdp[[3]] = dp[dp$tscncat %in% 'Split-3',]
batchdp[[4]] = dp[dp$tscncat %in% 'Split-2',]
genera = ep[ep$Node %in% 'Genera_Presence',]
loc = ep[ep$Node %in% 'Location_Type',]
sedqual = ep[ep$Node %in% 'Sediment_Quality',]
burial = ep[ep$Node %in% 'Accumulated_Burial',]
ebatchind = 2

for(i in 1:nrow(physstatus)) {
  for(j in 1:length(batchdp)) {
    dpsub = dp[dp$tscncat %in% batchdp[[j]]$tscncat,] # Get subset of dp for this batch
    uniqscn = unique(dpsub$tscn)
    for(k in 1:nrow(genera)) {
      evid = dbn$evid # Initialise evid to empty
      for(l in 1:nrow(sedqual)) {
        for(m in 1:nrow(burial)) {
          for(n in 1:length(uniqscn)) { # foreach dredge scenario
            # Create unique scenario id
            sedquallabel = sedqual$SelectState[l]
            if(sedqual$SoftEvidence[l]=='0.5, 0.5') {
              sedquallabel = '50/50'
            }
            buriallabel = burial$SelectState[m]
            if(burial$SoftEvidence[m]=='0.5, 0.5') {
              buriallabel = '50/50'
            }
            scnid = paste('Phys',physstatus$SelectState[i],genera$SelectState[k],
                          'SQ',sedquallabel,'Burial',buriallabel,uniqscn[n],sep='-')
            
            # Add evidence
            evid=editevid('add',evid,c(0,scnid,'Realised_Shoot_Density',globstart,globstart,
                                       'High','NA'),NA)
            evid=editevid('add',evid,c(0,scnid,'Realised_Biomass',globstart,globstart,
                                       'High','NA'),NA)
            evid=editevid('add',evid,c(0,scnid,'Genera_Presence',globstart,globend,
                                       genera$SelectState[k],'NA'),NA)
            evid = editevid('add',evid,c(0,scnid,'Location_Type',globstart,globend,
                                         loc$SelectState[k],'NA'),NA)
            # ASSUME: no burial, sediment quality effect or light impact outside dredging windows
            evid = editevid('add',evid,c(0,scnid,'Sediment_Quality',globstart,globend,
                                         'NoEffect','NA'),NA)
            evid = editevid('add',evid,c(0,scnid,'Accumulated_Burial',globstart,globend,
                                         'NoEffect','NA'),NA)
#             evid = editevid('add',evid,c(0,scnid,'Accumulated_Light',globstart,globend,
#                                          'AboveSaturation','NA'),NA)
            if(genera$SelectState[k]=='Amphibolis') { 
              evid = setevidbypattern(evid,scnid,'Accumulated_Light',globstart,globend,1:12,
                                      lamphibolis,timeunit)
            } else if(genera$SelectState[k]=='Halophila') {
              evid = setevidbypattern(evid,scnid,'Accumulated_Light',globstart,globend,1:12,
                                      lhalophila,timeunit)
            }
            # Add dredging effects on: burial, sediment quality and light
            dpscn = dpsub[dpsub$tscn %in% uniqscn[n],]
            for(n2 in 1:nrow(dpscn)) { # foreach row in this scenario
              if(dpscn$Dredge[n2]) {
                evid = editevid('add',evid,c(0,scnid,'Sediment_Quality',dpscn$Start[n2],
                                             dpscn$End[n2],sedqual$SelectState[l],
                                             sedqual$SoftEvidence[l]),NA)
                evid = editevid('add',evid,c(0,scnid,'Accumulated_Burial',dpscn$Start[n2],
                                             dpscn$End[n2],burial$SelectState[m],
                                             burial$SoftEvidence[m]),NA)
#                 evid = editevid('add',evid,c(0,scnid,'Accumulated_Light',dpscn$Start[n2],
#                                              dpscn$End[n2],'BelowSaturation','NA'),NA)
                if(genera$SelectState[k]=='Amphibolis') { 
                  evid = setevidbypattern(evid,scnid,'Accumulated_Light',dpscn$Start[n2],
                                          dpscn$End[n2],1:12,ldamphibolis,timeunit)
                } else if(genera$SelectState[k]=='Halophila') {
                  evid = setevidbypattern(evid,scnid,'Accumulated_Light',dpscn$Start[n2],
                                          dpscn$End[n2],1:12,ldhalophila,timeunit)
                }
                if(n2==1){ # Add phys status initialisation
                  evid = editevid('add',evid,c(0,scnid,'Physiological_Status_of_Plants',
                                               dpscn$Start[n2],dpscn$Start[n2],
                                               physstatus$SelectState[i],'NA'),NA)
                }
              }
            } # end foreach row in this scenario
          } # end foreach dredge scenario
        } # end foreach burial
      } # end foreach sedqual
      ebatchind = ebatchind + 1
      ebatch[[ebatchind]] = evid
    } # end foreach genera/loc
  } # end foreach batch
} # end foreach phys status initialisation

et[nrow(et)+1,1:2] = c(proc.time()[[3]],'genevid') # For timing

# --------------------------------------------------------------------------------------------------
# Run the simulations!!
source('customiseplot.R')
# Outputs to save are: ebatch (list of evid structures), and dbatch (list of dbn structures)
# plotnodes=c('Sediment_Quality','Accumulated_Burial','Accumulated_Light',
#             'Physiological_Status_of_Plants','Lateral_Growth_from_Existing_Individuals',
#             'Overall_Lateral_Growth','Seed_Production_Status','Seed_Density',
#             'Recruitment_Rate_from_Seeds','Ability_to_Recover','Ability_to_Resist_Hazard',
#             'Loss_in_Shoot_Density','Rate_of_Recovery_in_Shoot_Density','Net_Change_Shoot_Density',
#             'Realised_Shoot_Density','Baseline_Shoot_Density',
#             'Loss_in_Biomass','Rate_of_Recovery_in_Biomass','Net_Change_Biomass',
#             'Realised_Biomass','Baseline_Biomass',
#             'Loss_in_Aerial_Extent','Rate_of_Recovery_in_Aerial_Extent','Change_in_Aerial_Extent')

plotnodes=c('Realised_Shoot_Density','Baseline_Shoot_Density',
            'Loss_in_Shoot_Density','Rate_of_Recovery_in_Shoot_Density','Net_Change_Shoot_Density',
            'Recruitment_Rate_from_Seeds','Overall_Lateral_Growth',
            'Physiological_Status_of_Plants',
            'Lateral_Growth_from_Existing_Individuals','Seed_Production_Status','Seed_Density',
            'Ability_to_Recover','Ability_to_Resist_Hazard',
            'Sediment_Quality','Accumulated_Burial','Accumulated_Light',
            'Loss_in_Biomass','Rate_of_Recovery_in_Biomass','Net_Change_Biomass',
            'Realised_Biomass','Baseline_Biomass',
            'Loss_in_Aerial_Extent','Rate_of_Recovery_in_Aerial_Extent','Change_in_Aerial_Extent')
save(ebatch,file='testScenarios_evid.RData')
dbatch = list()
dbnorig = dbn

# cl<-makeCluster(6) #change the 2 to your number of CPU cores
# registerDoSNOW(cl)
# out = foreach(i=1:6) %dopar% {
for(i in 1:length(ebatch)) {
  dbn = dbnorig
  dbn$evid = ebatch[[i]]
  
  # --- Run
  et[nrow(et)+1,1:2] = c(proc.time()[[3]],sprintf('refrun%d',i)) # For timing
  E = ebatch[[i]]
#   E = E[E$Scenario %in% 'Phys-Good-Amphibolis-SQ-NoEffect-Burial-NoEffect-12month-Jan',]
#   E = E[E$Scenario %in% 'Phys-Good-Halophila-SQ-NoEffect-Burial-NoEffect-12month-Jan',]
# E = ebatch[[1]][1:92,]
# E = ebatch[[2]][1:92,]
  dbn = rundbn(dbn,E,8,'Time_of_Year',dbn$datetype) 
  dbn$posterior = fastwmean(dbn$posterior,dbn,c(0,0.25,0.5,0.75,1))
  et[nrow(et)+1,1:2] = c(proc.time()[[3]],sprintf('run%d',i)) # For timing
  
  # --- Plot
  et[nrow(et)+1,1:2] = c(proc.time()[[3]],sprintf('refplot%d',i)) # For timing
  objplot = dbnplot(dbn$posterior,plotnodes,'statebeliefs','%m-%Y')
  dbn$lineplots = customiseplot(objplot,plotnodes)
  for(j in 1:length(dbn$lineplots)) {
    if(!is.null(dbn$lineplots[[j]])) {
      dbn$lineplots[[j]] = dbn$lineplots[[j]] + facet_wrap(~scenario,ncol=4)
    }
  }
  dbn$contourplots = dbnplot(dbn$posterior,plotnodes,'wmean','%m-%Y')
  for(j in 1:length(dbn$contourplots)) {
    if(!is.null(dbn$contourplots[[j]])) {
      dbn$contourplots[[j]] = dbn$contourplots[[j]] + facet_wrap(~scenario,ncol=4)
    }
  }
  et[nrow(et)+1,1:2] = c(proc.time()[[3]],sprintf('plot%d',i)) # For timing
  
  # --- Save
  et[nrow(et)+1,1:2] = c(proc.time()[[3]],sprintf('refsave%d',i)) # For timing
  dbatch[[i]] = dbn
  #save(ebatch,dbatch,file='testScenarios.RData')
  save(dbn,file=sprintf('testScenarios_dbn%02d.Rdata',i))
  et[nrow(et)+1,1:2] = c(proc.time()[[3]],sprintf('save%d',i)) # For timing

  # --- Save plots as pdf
  w = 15.53 #7*2.5 # Use A3 size with 1in margins all round
  h = 10.69 #7*9/16*2.5
  et[nrow(et)+1,1:2] = c(proc.time()[[3]],sprintf('refpdf%d',i)) # For timing
  xplot1 = dbn$lineplots
  xplot2 = dbn$contourplots

  pdf(paste(basename,sprintf('%02d.pdf',i),sep=''),width=w,height=h,points=12)
  for(j in 1:length(dbn$lineplots)) {
    xplot1[[j]] = xplot1[[j]] + theme(text=element_text(size=10),
                                      axis.text.x=element_text(size=10))
    xplot1[[j]]$layers[[1]] = NULL
    xplot1[[j]] = xplot1[[j]] + geom_line(size=0.5)
    bequiet = lapply(xplot1[j],print)
    if(!is.null(dbn$contourplots[[j]])) {
      xplot2[[j]] = xplot2[[j]] + theme(text=element_text(size=10),
                                        axis.text.x=element_text(size=10))
      bequiet = lapply(xplot2[j],print)
    }
  }
  dev.off()
  et[nrow(et)+1,1:2] = c(proc.time()[[3]],sprintf('pdf%d',i)) # For timing

  sprintf('Completed batch %d of %d',i,length(ebatch))
  et
}
# stopCluster(cl)

####################################################### TIMING
et$t = as.numeric(et$t)
et$dt[2:nrow(et)] = diff(et$t)
save(et,file='testScenarios_timing.Rdata')
# For comparison, at work computer:
# t description
# 1 26.30   refnewdbn
# 2 28.12      newdbn
# 3 28.14 refrestruct
# 4 28.17    restruct
# 5 29.31  refsetevid
# 6 29.60     setevid


# windows()
# dbn$contourplots[[15]] # shoot den
# # ggsave(paste(basename,'Shoot.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[15]] # shoot den pr
# # ggsave(paste(basename,'Shoot-pr.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[16]] # base
# # ggsave(paste(basename,'base.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$contourplots[[16]] # basel
# # ggsave(paste(basename,'basel.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[13]] # recov
# # ggsave(paste(basename,'recov.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$contourplots[[13]] # recovl
# # ggsave(paste(basename,'recovl.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[12]] # loss
# # ggsave(paste(basename,'loss.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$contourplots[[12]] # lossl
# # ggsave(paste(basename,'lossl.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[14]] # net change
# # ggsave(paste(basename,'loss.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$contourplots[[20]] # biomass
# # ggsave(paste(basename,'biomass.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[4]] # phys
# # ggsave(paste(basename,'phys.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[8]] # seedden
# # ggsave(paste(basename,'seedd.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[6]] # lat
# # ggsave(paste(basename,'lat.pdf',sep='-'),width=w,height=h,scale=1.4)
# 
# windows(width=w,height=h)
# dbn$lineplots[[9]] # recruit rate
# # ggsave(paste(basename,'recruit.pdf',sep='-'),width=w,height=h,scale=1.4)


# Test uncompressed save - is it quicker?
# Yes, 97.11s for 10.1GB file! 202s for compressed 476MB file!!
# load('testScenarios.Rdata')
# et[nrow(et)+1,1:2] = c(proc.time()[[3]],'refsaverds') # For timing
# saveRDS(dbatch,file='testScenarios_dbatch.RDS',compress=TRUE)
# et[nrow(et)+1,1:2] = c(proc.time()[[3]],'saverds') # For timing
# et[nrow(et)+1,1:2] = c(proc.time()[[3]],'refsepsave') # For timing
# for(i in 1:length(dbatch)) {
#   dbn = dbatch[[i]]
#   save(dbn,file=sprintf('testScenarios_dbn%02d.Rdata',i))
# }
# et[nrow(et)+1,1:2] = c(proc.time()[[3]],'sepsave') # For timing

# Test save as plot - save biomass only
# et[nrow(et)+1,1:2] = c(proc.time()[[3]],'refsavepdf') # For timing
# for(i in 1:length(dbatch)) {
#   dbn = dbatch[[i]]
#   ggsave(sprintf('Realised_Biomass_Batch%02d.pdf',i),plot=dbn$contourplots[[20]],width=40.0,
#          height=20,units='cm',scale=1.75)
# }
# et[nrow(et)+1,1:2] = c(proc.time()[[3]],'savepdf') # For timing

load('Q:/Work/AIMS Collaboration/Models and Data/Seagrass Model/Archive/DBNSMILER_20141202_Scenarios_LightPatternOffset_Fixed/WithLightData/testScenarios_dbn07.RData')
P = dbn$posterior
P2 = P[P$scenario %in% 'Phys-Good-Amphibolis-SQ-NoEffect-Burial-NoEffect-Split-3-Oct',]
a = dbnplot(P2,'Rate_of_Recovery_in_Shoot_Density','wmean','%m-%Y')
a[[1]] = a[[1]] + coord_cartesian(xlim=as.Date(c('2015-7-1','2016-7-1')))
b = dbnplot(P2,'Realised_Shoot_Density','wmean','%m-%Y')
b[[1]] = b[[1]] + coord_cartesian(xlim=as.Date(c('2015-7-1','2016-7-1')),ylim=c(25,75))
c = dbnplot(P2,'Realised_Shoot_Density','wmean','%m-%Y')
c[[1]] = c[[1]] + coord_cartesian(xlim=as.Date(c('2016-7-1','2019-7-1')),ylim=c(5,60))
c[[1]] = c[[1]] + geom_rect(aes(xmin=as.Date('2017-10-1'),xmax=as.Date('2017-12-31'),ymin=0,ymax=100),
          fill=0x66B2FF, alpha=0.005)
c[[1]] = c[[1]] + geom_rect(aes(xmin=as.Date('2018-4-1'),xmax=as.Date('2018-6-30'),ymin=0,ymax=100),
                            fill=0x66B2FF, alpha=0.005)
P3 = P[P$scenario %in% 'Phys-Good-Amphibolis-SQ-NoEffect-Burial-NoEffect-Split-3-Jul',]
d = dbnplot(P3,'Realised_Shoot_Density','wmean','%m-%Y')
d[[1]] = d[[1]] + coord_cartesian(xlim=as.Date(c('2017-1-1','2020-1-1')),ylim=c(10,70))
d[[1]] = d[[1]] + geom_rect(aes(xmin=as.Date('2017-7-1'),xmax=as.Date('2017-9-30'),ymin=0,ymax=100),
                            fill=0x66B2FF, alpha=0.005)
d[[1]] = d[[1]] + geom_rect(aes(xmin=as.Date('2018-1-1'),xmax=as.Date('2018-3-31'),ymin=0,ymax=100),
                            fill=0x66B2FF, alpha=0.005)
P4 = P[P$scenario %in% 'Phys-Good-Amphibolis-SQ-NoEffect-Burial-NoEffect-Split-3-Jan',]
e = dbnplot(P4,'Realised_Shoot_Density','wmean','%m-%Y')
e[[1]] = e[[1]] + coord_cartesian(xlim=as.Date(c('2016-7-1','2019-7-1')),ylim=c(5,60))
e[[1]] = e[[1]] + geom_rect(aes(xmin=as.Date('2017-1-1'),xmax=as.Date('2017-3-31'),ymin=0,ymax=100),
                            fill=0x66B2FF, alpha=0.005)
e[[1]] = e[[1]] + geom_rect(aes(xmin=as.Date('2017-7-1'),xmax=as.Date('2017-9-30'),ymin=0,ymax=100),
                            fill=0x66B2FF, alpha=0.005)
P5 = P[P$scenario %in% 'Phys-Good-Amphibolis-SQ-NoEffect-Burial-NoEffect-Split-3-Apr',]
f = dbnplot(P5,'Realised_Shoot_Density','wmean','%m-%Y')
f[[1]] = f[[1]] + coord_cartesian(xlim=as.Date(c('2017-1-1','2020-1-1')),ylim=c(10,70))
f[[1]] = f[[1]] + geom_rect(aes(xmin=as.Date('2017-4-1'),xmax=as.Date('2017-6-30'),ymin=0,ymax=100),
                            fill=0x66B2FF, alpha=0.005)
f[[1]] = f[[1]] + geom_rect(aes(xmin=as.Date('2017-10-1'),xmax=as.Date('2017-12-31'),ymin=0,ymax=100),
                            fill=0x66B2FF, alpha=0.005)








