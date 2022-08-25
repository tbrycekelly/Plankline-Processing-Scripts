library(data.table)
library(archive)
source('R/scripts.R')


#### Project preparation and parameter setting

#base.dir = '/media/plankline/Data/Data/2022-07-22_17-18-24.796/'
base.dir = '/media/plankline/Data/Data/2022-07-21_23-26-04.102/'
out.dir = paste0(base.dir, 'R/')

frame.rate = 20.5
bin.width = 2 # second(s)
p.threshold = 0.8 # probability threshold



## First merge classification + measurements. Then load generate binned summary:
class.file = list.files(paste0(out.dir, 'classification/'),
                        pattern = '.rds',
                        full.names = T)


if (!dir.exists(paste0(out.dir, 'bin/'))) { dir.create(paste0(out.dir, 'bin/')) }

for (i in 1:length(class.file)) {
  message('Merging file ', i, ' of ', length(class.file), '... ')
  class = readRDS(class.file[i])
  
  ## Add timestamp
  temp = strsplit(class$image[1], split = '-')[[1]]
  class$time = as.POSIXct(paste0(temp[3], '-', temp[4], '-', temp[5], ' ', temp[6], ':', temp[7], ':', temp[8]), tz = 'UTC') + (class$frame - 1) / frame.rate
  
  ## Load measurements and add them if "they exist" and "number of entries matches"
  measure.file = gsub('classification', 'measurement', class.file[i])
  
  if (file.exists(measure.file)) {
    measure = readRDS(measure.file)
    
    ## This "should" always be true:
    if (nrow(measure) == nrow(class)) {
      
      ## Add necessary columns from measurement to classification dataframe.
      for (n in colnames(measure)[!colnames(measure) %in% colnames(class)]) {
        class[[n]] = measure[[n]]
      }
      
      ## bin
      class$time = as.POSIXct(round(as.numeric(class$time) / bin.width) * bin.width, origin = '1970-01-01')
      bin = data.frame(time = unique(class$time))
      
      bin$frames = sapply(bin$time, function(x) {diff(range(class$frame[class$time == x]))})
      
      for (n in unique(class$class)) { ## Already normalized to number of frames!
        bin[[n]] = sapply(bin$time, function(x) {sum(class$time == x & class$class == n & class$p >= p.threshold)}) / bin$frames 
      }
      
      ## output
      saveRDS(bin, file = paste0(out.dir, 'bin/', class$image[1], ' bin.rds'))
      
    } else {
      message('Uneven number of entries in classification and measurement files for a file! This is bad.')
    }
    
  } else {
    message('No Measurement file associated with ', class.file[i], '!!! This is bad.')
  }
}






## load binned data (example of how to do it reliably):
if (F) {
  bin.file = list.files(paste0(out.dir, 'bin/'),
                        pattern = '.rds',
                        full.names = T)
  
  bin = data.frame(time = 0)
  
  for (i in 1:length(bin.file)) {
    message(i)
    temp = readRDS(bin.file[i])
    
    bin.colnames = colnames(bin)
    temp.colnames = colnames(temp)
    
    for (n in temp.colnames) {
      if (!n %in% bin.colnames) {
        bin[[n]] = NA
      }
    }
    
    for (n in colnames(bin)) {
      if (!n %in% temp.colnames) {
        temp[[n]] = NA
      }
    }
    
    ## Now merge!
    bin = rbind(bin, temp)
    
  }
  
  bin = bin[-1,]
  
  saveRDS(bin, paste0(out.dir, 'bin.RDS'))
}
