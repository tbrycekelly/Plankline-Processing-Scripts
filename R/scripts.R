
load.classifications = function(file, verbose = T) {
  
  if (verbose) {message('Attempting to read file ', file)}
  class.data = fread(file)
  
  if (verbose) { message('\tFound ', nrow(class.data), ' entries. Processing... ', appendLF = F)}
  
  class.summary = data.frame(image = rep(NA, nrow(class.data)), frame = NA, roi = NA, class = NA, p = NA)
  
  if (verbose) { message('Identifying image and frame... ', appendLF = F)}
  temp = strsplit(class.data$image[1], split = '/|_')[[1]]
  class.summary$image = paste(temp[length(temp)-4], temp[length(temp)-3], sep = '_')
  
  temp = strsplit(class.data$image, split = "\\.|_")
  class.summary$frame = sapply(1:length(temp), function (x) { temp[[x]][length(temp[[x]])-3] })
  class.summary$roi = sapply(1:length(temp), function (x) { temp[[x]][length(temp[[x]])-1] })
  
  class.summary$frame = as.numeric(class.summary$frame)
  class.summary$roi = as.numeric(class.summary$roi)
  
  if (verbose) { message('Calculating classification... ', appendLF = F)}
  names = colnames(class.data)[-1] # All column names without the first one (i.e. "image")
  class.summary$class = names[apply(class.data[,-1], 1, which.max)]
  class.summary$p = apply(class.data[,-1], 1, function (x) { max(x) / sum(x) })
  if (verbose) { message('Done.') }
  
  ## Return
  class.summary
}




load.measurements = function(file.path, verbose = T) {
  ## Determine files in the given archive. Find the csv file
  files = as.data.frame(archive::archive(file.path))
  l = which(grepl(files$path[1:10], pattern = '.csv'))
  
  if (length(l) < 1) {
    message('No csv file found in ', file.path, '!!!')
    return()
  }
  
  ## Load the csv file from inside the archive
  data = read.csv(archive::archive_read(archive = file.path, file = files$path[l]))
  
  ## setup final data structure and then populate with data
  measurement.summary = data.frame(image = rep(NA, nrow(data)),
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
  if (verbose) { message('Done.') }
  
  ## Return 
  measurement.summary
}










