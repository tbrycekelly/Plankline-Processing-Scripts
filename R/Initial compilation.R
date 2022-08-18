library(data.table)
library(archive)
source('R/scripts.R')


#### Project preparation and parameter setting

base.dir = '/media/plankline/Data/Data/2022-07-22-17-18-24.796/'
out.dir = paste0(base.dir, 'R/')
if (!dir.exists(out.dir)) { dir.create(out.dir)}


#### Classification files
# In this script, we will load data from each classification file, parse it into a summary data structure, and then save the results to an RDS file
# in the output directory. This is primarily a preprocessing step. Log files are similarly loaded, parsed, and a summary is saved for the entire 
# project into the output folder.

class.file = list.files(path = paste0(base.dir, 'classification/'),
                        pattern = '*.csv',
                        full.names = T)

class.log = list.files(path = paste0(base.dir, 'classification/'),
                       pattern = '*.log',
                       full.names = T)

measure.file = list.files(path = paste0(base.dir, 'segmentation/'),
                          pattern = '*.tar.gz',
                          full.names = T)



## Preprocess the classification files here:
if (!dir.exists(paste0(out.dir, 'classification/'))) { dir.create(paste0(out.dir, 'classification/')) }

for (i in 1:length(class.file)) {
  ## Load file
  class.summary = load.classifications(class.file[i])
  
  ## Save summary file
  saveRDS(class.summary, file = paste0(out.dir, 'classification/', class.summary$image[1], ' classification.rds'))
  
  plot(density(class.summary$p),
       xlim = c(0, 1),
       ylim = c(0,2.5),
       yaxs = 'i',
       xaxs = 'i',
       lwd = 3,
       main = class.summary$image[1])
}


## Preprocess and summarize log files here:





## Preprocess measurement files here:
if (!dir.exists(paste0(out.dir, 'measurement/'))) { dir.create(paste0(out.dir, 'measurement/')) }

for (i in 1:length(measure.file)) {
  ## Load file
  summary = load.measurements(measure.file[i])
  
  ## Save summary file
  saveRDS(summary, file = paste0(out.dir, 'measurement/', summary$image[1], ' measurement.rds'))
  
  plot(density(log10(summary$area)),
       yaxs = 'i',
       xaxs = 'i',
       lwd = 3,
       main = summary$image[1])
}



## First merge classification+ measurements. Then load generate binned summary:
class.file = list.files(paste0(out.dir, 'classification/'),
                        pattern = '.rds',
                        full.names = T)

part = data.frame()
for (i in 1:length(class.file)) {
  message('Starting file ', i, ' of ', length(class.file), '... ')
  class = readRDS(class.file[i])
  
  ## Add timestamp
  temp = strsplit(class$image, split = '-')
  class$time = sapply(1:length(temp),
                      function(x) {as.POSIXct(paste0(temp[[x]][3], '-', temp[[x]][4], '-', temp[[x]][5], ' ', temp[[x]][6], ':', temp[[x]][7], ':', temp[[x]][8]), tz = 'UTC')})
  
  ## Load measurements and add them if "they exist" and "number of entries matches"
  measure.file = gsub('classification', 'measurement', class.file[i])
  
  if (file.exists(measure.file)) {
    measure = readRDS(measure.file)
    
    if (nrow(measure) == nrow(class)) {
      
      ## Add necessary columns from measurement to calssificcation dataframe.
      for (n in colnames(measure)[!colnames(measure) %in% colnames(class)]) {
        class[[n]] = measure[[n]]
      }
      
      ## Merge data into particle dataframe
      part = rbind(part, class)
      
    } else {
      message('Uneven number of entries in classification and measurement files for a file! This is bad.')
    }
    
  } else {
    message('No Measurement file associated with ', class.file[i], '!!!')
  }
}

temp = strsplit(part$image, split = '-')
part$time = sapply(1:length(temp),
                   function(x) {as.POSIXct(paste0(temp[[x]][3], '-', temp[[x]][4], '-', temp[[x]][5], ' ', temp[[x]][6], ':', temp[[x]][7], ':', temp[[x]][8]), tz = 'UTC')})



## Bin










