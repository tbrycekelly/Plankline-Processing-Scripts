library(data.table)
library(archive)
source('R/scripts.R')


#### Project preparation and parameter setting

#base.dir = '/media/plankline/Data/Data/2022-07-22_17-18-24.796/'
base.dir = '/media/plankline/Data/Data/2022-07-21_23-26-04.102/'
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



#### Preprocess measurement files here:
if (!dir.exists(paste0(out.dir, 'measurement/'))) { dir.create(paste0(out.dir, 'measurement/')) }

for (i in 1:length(measure.file)) {
  message('Processing measurement file ', i, ' of ', length(measure.file), ' (', round(i*100/length(measure.file)), '%)')
  
  ## Load file
  summary = load.measurements(measure.file[i], verbose = F)
  
  ## Save summary file
  saveRDS(summary, file = paste0(out.dir, 'measurement/', summary$image[1], ' measurement.rds'))
  
  plot(density(log10(summary$area)),
       yaxs = 'i',
       xaxs = 'i',
       lwd = 3,
       main = summary$image[1])
}



