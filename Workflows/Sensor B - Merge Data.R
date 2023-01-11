library(openxlsx)
library(geosphere) # for distance calculations

gps = readRDS('_rdata/gps.rdata')
ctd1 = readRDS('_rdata/ctd 1.rdata')
ctd2 = readRDS('_rdata/ctd 2.rdata')
#eng = readRDS('_rdata/engineering ')
fluoro1 = readRDS('_rdata/fluorometer 1.rdata')
fluoro2 = readRDS('_rdata/fluorometer 2.rdata')
analog = readRDS('_rdata/analog.rdata')


## bin time

dt = 1 #seconds

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
  dpi[[n]] = approx(as.numeric(gps$Time), gps[,n], xout = as.numeric(dpi$Time), ties = mean)$y
}

## CTD
for (n in names(ctd1)[-1]) {
  dpi[[n]] = approx(as.numeric(ctd1$Time), ctd1[[n]], xout = as.numeric(dpi$Time), ties = mean)$y
}
for (n in names(ctd2)[-1]) {
  dpi[[n]] = approx(as.numeric(ctd2$Time), ctd2[[n]], xout = as.numeric(dpi$Time), ties = mean)$y
}

## Analog
for (n in names(analog)[2:5]) {
  dpi[[n]] = approx(as.numeric(analog$Time), analog[[n]], xout = as.numeric(dpi$Time), ties = mean)$y
}

##Fluorometer
for (n in names(fluoro1)[c(3,5,7)]) {
  dpi[[n]] = approx(as.numeric(fluoro1$Time), fluoro1[[n]], xout = as.numeric(dpi$Time), ties = mean)$y
}
for (n in names(fluoro2)[c(3,5,7)]) {
  dpi[[n]] = approx(as.numeric(fluoro2$Time), fluoro2[[n]], xout = as.numeric(dpi$Time), ties = mean)$y
}

## ENG
#for (n in names(eng)[-1]) {
#  dpi[[n]] = approx(as.numeric(eng$Time), eng[[n]], xout = as.numeric(dpi$Time), ties = mean)$y
#}


#### Filter
dpi = dpi[dpi$Pressure > 2,]

# Break at time gaps
k = c(0, which(diff(dpi$Time) > 3600), nrow(dpi))

transects = list()

for (i in 2:length(k)) {
  transects[[paste0('dpi', i-1)]] = dpi[(k[i-1]+1):k[i],]  
  
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

saveRDS(transects, file = '_rdata/transects.rdata')
saveRDS(dpi, file = '_rdata/dpi.rdata')

for (i in 1:length(transects)) {
  #write.xlsx(transects[[i]], file = paste0('Publish/Transect ', gsub(':', '', transects[[i]]$Time[1]), '.xlsx'))
  write.xlsx(transects[[i]], file = paste0('S:/Publish/Transect ', gsub(':', '', transects[[i]]$Time[1]), '.xlsx'))
  saveRDS(transects[[i]], file = paste0('S:/Publish/Transect ', gsub(':', '', transects[[i]]$Time[1]), '.RDS'))
}
