
#' @export
process.measurements = function(base.dir, out.dir) {
  
  measure.file = list.files(path = paste0(base.dir, '/segmentation/'),
                            pattern = '*.tar.gz',
                            full.names = T)
  
  #### Preprocess measurement files here:
  if (!dir.exists(paste0(out.dir, '/measurement/'))) { dir.create(paste0(out.dir, '/measurement/')) }
  
  pdf(paste0(out.dir, '/Measurement Area Density.pdf'))
  par(mfrow = c(2,2))
  for (i in 1:length(measure.file)) {
    message('Processing measurement file ', i, ' of ', length(measure.file), ' (', round(i*100/length(measure.file)), '%)')
    
    ## Load file
    summary = load.measurements(measure.file[i], verbose = F)
    
    ## Save summary file
    saveRDS(summary, file = paste0(out.dir, '/measurement/', summary$image[1], ' measurement.rds'))
    
    plot(density(log10(summary$area)),
         yaxs = 'i',
         xaxs = 'i',
         lwd = 3,
         main = summary$image[1])
  }
  dev.off()
  
  return()
}