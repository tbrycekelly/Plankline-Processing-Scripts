library(TheSource)
source('R/scripts.R')

base.dir = '/media/plankline/Data/Data/2022-07-21_23-26-04.102/'
#sensor = readRDS('/media/plankline/Data/Sensor/Transect 2022-07-22 022347.RDS')
sensor = readRDS('/mnt/shuttle/Publish/Transect 2022-07-22 022347.RDS')

sensor$Time = sensor$Time - 3600 * 8


#### Compile merged classification/sensor package dataset
bin.files = list.files(paste0(base.dir, '/R/bin/'), pattern = '.rds', full.names = T)
comp = data.frame(time = NA)

for (i in 1:length(bin.files)) {
  message('Merging binned data and sensor data for file ', i, ' of ', length(bin.files), '...')
  bin = readRDS(bin.files[i])
  
  ## check if sensor data is available
  if (sum(sensor$Time >= min(bin$time) & sensor$Time <= max(bin$time)) < 1) {
    message('ERROR! No sensor data found for file ', bin.files[i],'.')
  }
  
  ## Merge sensor data with class file
  new.cols = colnames(sensor)[!colnames(sensor) %in% colnames(bin)]
  if (length(new.cols) > 0) {
    for (n in new.cols) {
      bin[[n]] = approx(sensor$Time, sensor[[n]], xout = bin$time, rule = 2)$y
    }
  }
  
  ## determine if any filler columns need to be added:
  new.cols = colnames(bin)[!colnames(bin) %in% colnames(comp)]
  if (length(new.cols) > 0) {
    for (n in new.cols) {
      comp[[n]] = NA
    }
  }
  
  new.cols = colnames(comp)[!colnames(comp) %in% colnames(bin)]
  if (length(new.cols) > 0) {
    for (n in new.cols) {
      bin[[n]] = NA
    }
  }
  
  comp = rbind(comp, bin)
}

## Cleanup and save
comp = comp[-1,]
comp = comp[order(comp$time),]
comp$Time = NULL
comp[comp == Inf] = NA
comp = comp[, order(colnames(comp))]
comp$time = as.POSIXct(comp$time, origin = '1970-01-01')
saveRDS(comp, file = paste0(base.dir, '/R/compilation.rds'))





plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(log10(comp$copepod_cyclopoid_oithona), min = 0, max = 1, pal = 'parula'),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')

plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(log10(group(comp, 'copepod_')), min = 0, max = 1.5, pal = 'parula'),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')


plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(comp$Temperature, pal = 'parula'),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')

plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(comp$SigmaTheta, pal = 'parula'),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')

plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(comp$frames, pal = 'parula'),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')


plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(group(comp, 'unknown'), pal = 'parula', min = 0, max = 10),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')

plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(group(comp, 'appendicularian_'), pal = 'parula', min = 0, max = 5),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')


plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(group(comp, 'protist_'), pal = 'parula', min = 0, max = 1),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')


plot(x = comp$time,
     y = comp$Depth,
     col = make.pal(group(comp, 'detritus_'), pal = 'parula', min = 0, max = 100),
     pch = 20,
     ylim = c(120,0),
     yaxs = 'i')



section = build.section(x = comp$time,
                        y = comp$Depth,
                        z = data.frame(copepod = group(comp, 'copepod_'),
                                       detritus = group(comp, 'detritus_'),
                                       temperature = comp$Temperature,
                                       salinity = comp$Salinity,
                                       chl = comp$Chl, 
                                       oxy = comp$Oxygen),
                        lat = comp$Latitude,
                        lon = comp$Longitude, x.factor = 10)

plot.section(section, ylim = c(120,0), pal = 'parula', zlim = c(0, 50), xaxt = 'n', ylab = 'Depth (m)', xlab = '')
axis.POSIXct(1, comp$time)
add.section.contour(section, field = 'z3', col = 'red')
add.section.contour(section, field = 'z4', col = 'light blue')
add.section.contour(section, field = 'z5', col = 'green')
add.section.contour(section, field = 'z6')
mtext('Copepod_', adj = 0)

plot.section(section, field = 'z2', ylim = c(120,0), pal = 'parula', zlim = c(0, 50), xaxt = 'n', ylab = 'Depth (m)', xlab = '')
axis.POSIXct(1, comp$time)
#add.section.contour(section, field = 'z2')
add.section.contour(section, field = 'z3', col = 'red')
add.section.contour(section, field = 'z4', col = 'light blue')
add.section.contour(section, field = 'z5', col = 'green')
add.section.contour(section, field = 'z6')



map = make.map.nga()
add.map.points(map, lon = section$lon, lat = section$lat, col = 'dark red')
add.map.contour(map,
                bathy.pacific$Lon,
                bathy.pacific$Lat,
                z = bathy.pacific$Z,
                levels = c(-100, -1000), col = 'grey')
