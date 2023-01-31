library(data.table)
library(archive)
library(devtools)
library(PlanklinePS)


#### Project preparation and parameter setting
## Setup
root.dir = '/media/plankline/Data/Data/'

dirs = list.dirs(root.dir, recursive = F, full.names = T)
dirs = dirs[!grepl('test', dirs)]
#dirs = dirs[grepl('Done', dirs)]

for (base.dir in dirs) {
  
  out.dir = paste0(base.dir, '/R/')
  if (!dir.exists(out.dir)) { dir.create(out.dir)}
  
  frame.rate = 20.5
  bin.width = 2 # second(s)
  p.threshold = 0.5 # probability threshold
  
  
  #### Classification files
  # In this script, we will load data from each classification file, parse it into a summary data structure, and then save the results to an RDS file
  # in the output directory. This is primarily a preprocessing step. Log files are similarly loaded, parsed, and a summary is saved for the entire 
  # project into the output folder.
  
  class.file = list.files(path = paste0(base.dir, '/classification/'),
                          pattern = '*.csv',
                          full.names = T)
  
  class.log = list.files(path = paste0(base.dir, '/classification/'),
                         pattern = '*.log',
                         full.names = T)
  
  measure.file = list.files(path = paste0(base.dir, '/segmentation/'),
                            pattern = '*.tar.gz',
                            full.names = T)
  
  
  
  ## Preprocess the classification files here:
  if (!dir.exists(paste0(out.dir, '/classification/'))) { dir.create(paste0(out.dir, '/classification/')) }
  
  pdf(paste0(out.dir, '/Classification Probability Density.pdf'))
  par(mfrow = c(2,2))
  for (i in 1:length(class.file)) {
    ## Load file
    class.summary = load.classifications(class.file[i])
    
    ## Save summary file
    saveRDS(class.summary, file = paste0(out.dir, '/classification/', class.summary$image[1], ' classification.rds'))
    
    plot(density(class.summary$p),
         xlim = c(0, 1),
         ylim = c(0,2.5),
         yaxs = 'i',
         xaxs = 'i',
         lwd = 3,
         main = class.summary$image[1])
  }
  dev.off()
  
  ## Preprocess and summarize log files here:
  ## TODO: TBK
  
  
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
  
  
  ## First merge classification + measurements. Then load generate binned summary:
  class.file = list.files(paste0(out.dir, '/classification/'),
                          pattern = '.rds',
                          full.names = T)
  
  if (!dir.exists(paste0(out.dir, '/bin/'))) { dir.create(paste0(out.dir, '/bin/')) }
  
  for (i in 1:length(class.file)) {
    message('Binning file ', i, ' of ', length(class.file), '... ')
    class = readRDS(class.file[i])
    
    ## Add timestamp
    temp = strsplit(class$image[1], split = '-')[[1]]
    class$time = as.POSIXct(paste0(temp[3], '-', temp[4], '-', temp[5], ' ', temp[6], ':', temp[7], ':', temp[8]), tz = 'UTC') + (class$frame - 1) / frame.rate
    
    ## Load measurements and add them if "they exist" and "number of entries matches"
    #measure.file = gsub('classification', 'measurement', class.file[i])
    if (T) {
      #if (file.exists(measure.file)) {
      #measure = readRDS(measure.file)
      if (T) {
        ## This "should" always be true:
        #if (nrow(measure) == nrow(class)) {
        
        ## Add necessary columns from measurement to classification dataframe.
        #for (n in colnames(measure)[!colnames(measure) %in% colnames(class)]) {
        #  class[[n]] = measure[[n]]
        #}
        
        ## bin
        class$time = as.POSIXct(round(as.numeric(class$time) / bin.width) * bin.width, origin = '1970-01-01')
        bin = data.frame(time = unique(class$time))
        
        bin$frames = sapply(bin$time, function(x) {diff(range(class$frame[class$time == x]))})
        
        for (n in unique(class$class)) { ## Already normalized to number of frames!
          bin[[n]] = sapply(bin$time, function(x) {sum(class$time == x & class$class == n & class$p >= p.threshold)}) / bin$frames 
        }
        
        ## output
        saveRDS(bin, file = paste0(out.dir, '/bin/', class$image[1], ' bin.rds'))
        
      } else {
        message('Uneven number of entries in classification and measurement files for a file! This is bad.')
      }
      
    } else {
      message('No Measurement file associated with ', class.file[i], '!!! This is bad.')
    }
  }

}
