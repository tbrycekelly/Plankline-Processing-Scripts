library(data.table)
library(openxlsx)
library(TheSource)
library(PlanklinePS)


### GPS Files
gps.files = list.files('S:/SKQJ2022_data/VIPF_LOGS/GPS Publisher/',
                       full.names = T,
                       recursive = T,
                       pattern = '*.csv')
gps = load.dpi.files(gps.files)

colnames(gps) = c('Time', 'Message', 'datetime', 'Latitude', 'Hemisphere',
                  'Longitude', 'other hemi',
                  'v8', 'v9', 'v10', 'v11', 'v12', 'v13', 'v14','v15','vq6','v17','v18')

## Fix structure and pull out good data only
gps$Latitude = floor(gps$Latitude/100) + (gps$Latitude - floor(gps$Latitude/100)*100)/60
gps$Longitude = -1 * (floor(gps$Longitude/100) + (gps$Longitude - floor(gps$Longitude/100)*100)/60)
gps = gps[,c(1,4,6)]

gps = gps[gps$Time > make.time(2022, 6),]
saveRDS(gps, file = '_rdata/gps.rdata')



#### CTD 1
## Initial setup
ctd.files = list.files('S:/SKQJ2022_data/VIPF_LOGS/CTD 1 Publisher/',
                       full.names = T,
                       pattern = '*.csv',
                       recursive = T)

ctd = load.dpi.files(ctd.files)

colnames(ctd) = c('Time', 'Temperature', 'Conductivity', 'Pressure', 'Depth', 'Salinity', 'Sound.Velocity')


## Calculate
ctd$SigmaTheta = calc.sigma.theta(S = ctd$Salinity, Tmp = ctd$Temperature, P = ctd$Pressure, verbose = F)

ctd = ctd[ctd$Time > make.time(2022, 6),]
saveRDS(ctd, file = '_rdata/ctd 1.rdata')


#### CTD 2
## Initial setup
ctd.files = list.files('S:/SKQJ2022_data/VIPF_LOGS/CTD 2 Publisher/', full.names = T, pattern = '*.csv', recursive = T)
ctd = load.dpi.files(ctd.files)
ctd = ctd[ctd$Time > make.time(2020, 6),]

colnames(ctd) = c('Time', 'Temperature2', 'Conductivity2', 'Pressure2', 'Depth2', 'Salinity2', 'Sound.Velocity2')

## Calculate
ctd$SigmaTheta2 = calc.sigma.theta(S = ctd$Salinity2, Tmp = ctd$Temperature2, P = ctd$Pressure2, verbose = F)

saveRDS(ctd, file = '_rdata/ctd 2.rdata')



#### Analog
## Set up format
ad.files = list.files('S:/SKQJ2022_data/VIPF_LOGS/AD1216 Publisher/', full.names = T, pattern = '.csv', recursive = T)
ad = load.dpi.files(ad.files)

colnames(ad) = c('Time', 'Oxygen', 'PAR', 'pH', 'REDOX', 'Checksum', 'checksum2')

ad = ad[ad$Time > make.time(2022, 6),]
saveRDS(ad, file = '_rdata/analog.rdata')



#### Fluorometer 1
fl.files = list.files('S:/SKQJ2022_data/VIPF_LOGS/Fluorometer 1 Publisher/',
                      full.names = T,
                      recursive = T, 
                      pattern = '*.csv')
fl = load.dpi.files(fl.files)

colnames(fl) = c('Time', 'Chl.wave', 'Chl', 'FDOm.wave', 'FDOM', 'Phycocyanin.wave', 'Phycocyanin')

## Filter
fl$Chl[fl$Chl > 4e3 | fl$Chl < 10] = NA
fl$FDOM[fl$FDOM > 4e3 | fl$FDOM < 10] = NA
fl$Phycocyanin[fl$Phycocyanin > 4e3 | fl$Phycocyanin < 10] = NA

## Convert counts to real units
# Calibration from ???
fl$Chl = (fl$Chl - 49) * 0.0121
fl$FDOM = (fl$FDOM - 50) * 0.0914
fl$Phycocyanin = (fl$Phycocyanin - 46) * 0.0422

fl = fl[fl$Time > make.time(2022, 6),]
saveRDS(fl, file = '_rdata/fluorometer 1.rdata')




#### Fluorometer 2
fl.files = list.files('S:/SKQJ2022_data/VIPF_LOGS/Fluorometer 2 Publisher/',
                      full.names = T,
                      recursive = T,
                      pattern = '*.csv')
fl = load.dpi.files(fl.files)

colnames(fl) = c('Time', 'S470.wave', 'S470', 'S532.wave', 'S532', 'S650.wave', 'S650')

## Filter
fl$S470[fl$S470 > 4e3 | fl$S470 < 10] = NA
fl$S532[fl$S532 > 4e3 | fl$S532 < 10] = NA
fl$S650[fl$S650 > 4e3 | fl$S650 < 10] = NA

## Convert counts to real units
# Calibration from ???
fl$S470 = (fl$S470 - 35) * 0.00001181
fl$S532 = (fl$S532 - 50) * 0.000008216
fl$S650 = (fl$S650 - 50) * 0.00000411

fl = fl[fl$Time > make.time(2022, 6),]
saveRDS(fl, file = '_rdata/fluorometer 2.rdata')



#### Eng
eng.files = list.files('S:/SKQJ2022_data/VIPF_LOGS/Inclinometer Publisher/',
                       full.names = T,
                       pattern = '*.csv',
                       recursive = T)

eng = load.dpi.files(eng.files)

colnames(eng) = c('Time', 'AccelerationX', 'AccelerationY', 'AccelerationZ', 'Roll', 'Pitch', 'T')
eng = eng[eng$Time > make.time(2022, 6),]
saveRDS(eng, file = '_rdata/engineering.rdata')




