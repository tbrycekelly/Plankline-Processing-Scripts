library(openxlsx)
library(geosphere) # for distance calculations
library(PlanklinePS)
library(wql)

## Setup
save.dir = '/media/plankline/Data/Sensor/SKQJ2022_data/_rdata/'

gps = readRDS(paste0(save.dir, 'gps.rds'))
ctd1 = readRDS(paste0(save.dir, 'ctd1.rds'))
ctd2 = readRDS(paste0(save.dir, 'ctd2.rds'))
#eng = readRDS(paste0(save.dir, 'engineering.rds'))
fluoro1 = readRDS(paste0(save.dir, 'fluorometer1.rds'))
fluoro2 = readRDS(paste0(save.dir, 'fluorometer2.rds'))
analog = readRDS(paste0(save.dir, 'analog.rds'))
acs = readRDS(paste0(save.dir, 'acs.rds'))


## bin time

dt = 2 #seconds
ties = median

gps$Time = conv.time.unix(round(as.numeric(gps$Time) * dt) / dt)
ctd1$Time = conv.time.unix(round(as.numeric(ctd1$Time) * dt) / dt)
ctd2$Time = conv.time.unix(round(as.numeric(ctd2$Time) * dt) / dt)
#eng$Time = conv.time.unix(round(as.numeric(eng$Time) * dt) / dt)
fluoro1$Time = conv.time.unix(round(as.numeric(fluoro1$Time) * dt) / dt)
fluoro2$Time = conv.time.unix(round(as.numeric(fluoro2$Time) * dt) / dt)
analog$Time = conv.time.unix(round(as.numeric(analog$Time) * dt) / dt)


## Merging
dpi = data.frame(Time = unique(gps$Time))


## GPS
for (n in names(gps)[-1]) {
  dpi[[n]] = approx(as.numeric(gps$Time), gps[,n], xout = as.numeric(dpi$Time), ties = ties)$y
}

## CTD
for (n in names(ctd1)[-1]) {
  dpi[[n]] = approx(as.numeric(ctd1$Time), ctd1[[n]], xout = as.numeric(dpi$Time), ties = ties)$y
}
for (n in names(ctd2)[-1]) {
  dpi[[n]] = approx(as.numeric(ctd2$Time), ctd2[[n]], xout = as.numeric(dpi$Time), ties = ties)$y
}

## Analog
for (n in names(analog)[2:5]) {
  dpi[[n]] = approx(as.numeric(analog$Time), analog[[n]], xout = as.numeric(dpi$Time), ties = ties)$y
}

##Fluorometer
for (n in names(fluoro1)[c(3,5,7)]) {
  dpi[[n]] = approx(as.numeric(fluoro1$Time), fluoro1[[n]], xout = as.numeric(dpi$Time), ties = ties)$y
}
for (n in names(fluoro2)[c(3,5,7)]) {
  dpi[[n]] = approx(as.numeric(fluoro2$Time), fluoro2[[n]], xout = as.numeric(dpi$Time), ties = ties)$y
}

## ENG
#for (n in names(eng)[-1]) {
#  dpi[[n]] = approx(as.numeric(eng$Time), eng[[n]], xout = as.numeric(dpi$Time), ties = ties)$y
#}

## ACs
dpi[['a440']] = approx(as.numeric(acs$Time), acs$A440, xout = as.numeric(dpi$Time), ties = ties)$y
dpi[['a675']] = approx(as.numeric(acs$Time), acs$A675.8, xout = as.numeric(dpi$Time), ties = ties)$y
dpi[['ACS.index']] = approx(as.numeric(acs$Time), c(1:nrow(acs)), xout = as.numeric(dpi$Time), ties = ties)$y

#### Derived variables


#### Filter
dpi = dpi[dpi$Pressure > 2,]

## Outlier filtering
for (i in 1:ncol(dpi)) {
  dpi[,i] = runmed(dpi[,i], 3)
}

# Break at time gaps
k = c(0, which(diff(dpi$Time) > 3600), nrow(dpi))

transects = list()

for (i in 2:length(k)) {
  transects[[paste0('dpi', i-1)]] = dpi[(k[i-1]+1):k[i],]  
  
  
  
  ## Derived Variables
  index = min(round(transects[[i-1]]$ACS.index), na.rm = T):max(round(transects[[i-1]]$ACS.index), na.rm = T)
  
  if (diff(range(index)) > 1e3) {
    eo = wql::eof(acs[index,-1], n = 5)
    transects[[i-1]]$ACS.eof1 = approx(as.numeric(acs$Time[index]), eo$amplitude[,1], xout = as.numeric(transects[[i-1]]$Time), ties = ties)$y
    transects[[i-1]]$ACS.eof2 = approx(as.numeric(acs$Time[index]), eo$amplitude[,2], xout = as.numeric(transects[[i-1]]$Time), ties = ties)$y
  }
  
  ## Add section distance based on the cumulative sum of all the piecewise distances.
  transects[[paste0('dpi', i-1)]]$Distance = c(0,
                                               sapply(
                                                 c(2:nrow(transects[[paste0('dpi', i-1)]])),
                                                  function(x) {
                                                    geosphere::distCosine(p1 = c(transects[[paste0('dpi', i-1)]]$Longitude[x-1], transects[[paste0('dpi', i-1)]]$Latitude[x-1]),
                                                                          p2 = c(transects[[paste0('dpi', i-1)]]$Longitude[x], transects[[paste0('dpi', i-1)]]$Latitude[x]))
                                                  }) / 1e3)
  
  transects[[paste0('dpi', i-1)]]$Distance = cumsum(transects[[paste0('dpi', i-1)]]$Distance)
  
}

saveRDS(transects, file = paste0(save.dir, 'transects.rdata'))
saveRDS(dpi, file = paste0(save.dir, 'dpi.rdata'))

