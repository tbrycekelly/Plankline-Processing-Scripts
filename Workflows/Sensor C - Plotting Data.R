devtools::install_github('tbrycekelly/TheSource')
library(TheSource)

transects = readRDS('_rdata/transects.rdata')

#### MAPS

## CRP
map = make.map2(coast = 'coastline4',
                lon = -145,
                lat = 60,
                scale = 100,
                p = make.proj('nsper', lat= 50, lon = -145),
                dlon = 1,
                dlat = 1,
                land.col = 'black')

for (i in 1:length(transects)) {
  add.map.line(map, lon = transects[[i]]$Longitude, transects[[i]]$Latitude, greatCircle = F, col = pals::alphabet(8)[i], lwd = 5)
  add.map.text(map,
               text = i,
               lon = mean(transects[[i]]$Longitude, na.rm = T),
               lat = mean(transects[[i]]$Latitude, na.rm = T),
               pos = 2,
               col = 'blue')
}



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


pdf('Figures/SKQ202210S Section Plots (transect 1).pdf')
par(mfrow = c(2,1))
for (n in 1:ncol(section[[1]]$grid)) {
  message(n)
  plot.section(section[[1]], field = n,
               pal = 'inferno',
               ylim = c(100,0),
               mark.points = F,
               include.cex = 0.2,
               ylab = 'Depth',
               xlab = 'Section Distance',
               main = paste0('Transect 1 - ', names(section[[1]]$grid)[n]))
  
  add.section.bathy(section[[1]])
}

dev.off()


pdf('Figures/SKQ202210S Section Plots (transect 2).pdf')
par(mfrow = c(2,1))
for (n in 1:ncol(section[[2]]$grid)) {
  message(n)
  plot.section(section[[2]], field = n,
               pal = 'inferno',
               ylim = c(100,0),
               mark.points = F,
               include.cex = 0.2,
               ylab = 'Depth',
               xlab = 'Section Distance',
               main = paste0('Transect 2 - ', names(section[[2]]$grid)[n]))
}

dev.off()




## map if
map = make.map('coastlineWorldFine', lon.min = -150, lon.max = -145, lat.min = 57.8, lat.max = 60.3,
               dlon = 1, dlat = 1, land.col = '#333333')
#add.map.bathy.shade(map, bathy.global, pal = 'ocean.deep', zlim = c(-5e3, -100))
#redraw.map(map)
add.map.points(gps$Longitude, gps$Latitude)




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
