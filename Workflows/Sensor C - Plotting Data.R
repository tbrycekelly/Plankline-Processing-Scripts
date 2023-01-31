library(data.table)
library(archive)
library(devtools)
library(PlanklinePS)
library(TheSource)

## Setup
transects = readRDS('/media/plankline/Data/Sensor/SKQJ2022_data/_rdata/transects.rdata')

#### MAPS

## CRP
pdf(paste0(save.dir, 'SKQ202210S Maps.pdf'))
map = make.map2(coast = 'coastline4',
                lon = -147,
                lat = 59.5,
                scale = 200,
                p = make.proj('nsper', lat= 50, lon = -145),
                dlon = 2,
                dlat = 2,
                land.col = 'black')

for (i in 1:length(transects)) {
  col = pals::alphabet(length(transects))[i]
  add.map.line(map, lon = transects[[i]]$Longitude, transects[[i]]$Latitude, greatCircle = F, col = col, lwd = 5)
  add.map.text(map,
               text = i,
               lon = mean(transects[[i]]$Longitude, na.rm = T),
               lat = mean(transects[[i]]$Latitude, na.rm = T),
               pos = 2,
               col = col)
}


for (i in 1:length(transects)) {
  map = make.map2(coast = 'coastline3',
                  lon = -147,
                  lat = 59.5,
                  scale = 200,
                  p = make.proj('nsper', lat= 50, lon = -145),
                  dlon = 2,
                  dlat = 2,
                  land.col = 'black')
  
  col = pals::alphabet(length(transects))[i]
  add.map.line(map, lon = transects[[i]]$Longitude, transects[[i]]$Latitude, greatCircle = F, col = col, lwd = 5)
  add.map.text(map,
                text = i,
                lon = mean(transects[[i]]$Longitude, na.rm = T),
                lat = mean(transects[[i]]$Latitude, na.rm = T),
                pos = 2,
                col = col)
}
dev.off()

#### Build SECTIONS

section = list()

for (i in 1:length(transects)) {
  message(i)
  fields = names(transects[[i]])
  fields = fields[!fields %in% c('Distance', 'Depth')]
  section[[i]] = build.section(x = transects[[i]]$Distance,
                               y = transects[[i]]$Depth,  
                               z = transects[[i]][,fields],
                               field.names = fields,
                               ylim = c(0,100),
                               nx = 50,
                               ny = 100,
                               y.factor = 10,
                               uncertainty = 10,
                               neighborhood = 20,
                               lat = transects[[i]]$Latitude,
                               lon = transects[[i]]$Longitude,
                               gridder = gridBin,
                               verbose = F)
}


add.section.bathy = function(section, bathy = bathy.global, binning = 1, bathy.col = 'darkgrey') {
  
  
  ## Project lon/lat points
  p = make.proj(lat = median(section$lat), lon = median(section$lon))
  bathy$xy = rgdal::project(cbind(bathy$Lon, bathy$Lat), proj = p)
  section$xy = rgdal::project(cbind(section$lon, section$lat), proj = p)
  
  ## Calculate depth via bilinear interpolation
  depth = interp.bilinear(x = section$xy[,1], y = section$xy[,2], gx = bathy$xy[,1], gy = bathy$xy[,2], z = -as.numeric(bathy$Z))
  
  ## Filter
  depth = runmed(depth, binning)
  
  ## Draw polygon
  polygon(x = c(section$x, rev(section$x)), y = c(depth, rep(1e8, length(section$x))), col = bathy.col)
}


for (i in 1:length(transects)) {
  pdf(paste0(save.dir, 'SKQ202210S Section Plots (transect ', i,').pdf'))
  par(mfrow = c(2,1))
  for (n in 1:ncol(section[[i]]$grid)) {
    message(n)
    plot.section(section[[i]], field = n,
                 pal = 'inferno',
                 ylim = c(100,0),
                 mark.points = F,
                 include.cex = 0.2,
                 ylab = 'Depth',
                 xlab = 'Section Distance',
                 main = paste0('Transect ', i, ' - ', names(section[[i]]$grid)[n]))
    
    #add.section.bathy(section[[i]])
  }
  
  dev.off()
}


