#' @export
load.measurements = function(file.path, frame.rate = 20.5, verbose = T) {
  
  ## Get measurement file name
  f = strsplit(file.path, '/')[[1]]
  f = gsub(pattern = '.tar', replacement = '.csv', f[length(f)])
  f = paste0('./measurements/', f)
  
  ## Load the csv file from inside the archive
  data = utils::read.csv(archive::archive_read(archive = file.path, file = f))
  
  ## setup final data structure and then populate with data
  measurement.summary = data.frame(time = Sys.time(),
                                   image = rep(NA, nrow(data)),
                                   frame = NA,
                                   roi = NA,
                                   area = NA,
                                   major = NA,
                                   minor = NA,
                                   perimeter = NA,
                                   x = NA,
                                   y = NA,
                                   mean = NA,
                                   height = NA)
  
  if (verbose) { message('Identifying image and frame... ', appendLF = F)}
  
  temp = strsplit(data$image[1], split = '/|_')[[1]]
  measurement.summary$image = paste(temp[length(temp)-4], temp[length(temp)-3], sep = '_')
  
  temp = strsplit(data$image, split = "\\.|_")
  measurement.summary$frame = sapply(1:length(temp), function (x) { temp[[x]][length(temp[[x]])-3] })
  measurement.summary$roi = sapply(1:length(temp), function (x) { temp[[x]][length(temp[[x]])-1] })
  
  measurement.summary$frame = as.numeric(measurement.summary$frame)
  measurement.summary$roi = as.numeric(measurement.summary$roi)
  
  measurement.summary$area = data$area
  measurement.summary$major = data$major
  measurement.summary$minor = data$minor
  measurement.summary$perimeter = data$perimeter
  measurement.summary$x = data$x
  measurement.summary$y = data$y
  measurement.summary$mean = data$mean
  measurement.summary$height = data$height
  
  ## Get time
  temp = strsplit(measurement.summary$image[1], split = '-')[[1]]
  measurement.summary$time = as.POSIXct(paste0(temp[3], '-', temp[4], '-', temp[5], ' ', temp[6], ':', temp[7], ':', temp[8]), tz = 'UTC') + (measurement.summary$frame - 1) / frame.rate
  
  if (verbose) { message('Done.') }
  
  ## Return 
  measurement.summary
}