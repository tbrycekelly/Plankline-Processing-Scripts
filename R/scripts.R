
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


pad.number = function(x, pad = 4) {
  x = as.numeric(x)
  out = rep(NA, length(x))
  
  for (i in 1:length(x)) {
    dn = max(pad - nchar(x[i]), 0)
    out[i] = paste0(paste0(rep(0, dn), collapse = ''), x[i])
  }
  
  ## Return
  out
}



group = function(bin, pattern = '*', fun = function(x){sum(x, na.rm = T)}, verbose = T) {
  l = grep(pattern = pattern, x = colnames(bin))
  
  ## No matches, return NA
  if (length(l) < 1) {
    return (rep(NA, nrow(bin)))
  }
  if (length(l) == 1) {
    return(bin[,l])
  }
  
  if (verbose) {
    message('Applying function to ', length(l), ' columns:\n', paste0('\t', c(1:length(l)), ') ', colnames(bin)[l], collapse = '\n'))
  }
  
  ## Calculate sum, mean, etc.
  apply(bin[,l], 1, fun)
}



pull = function(project.dir, pattern = '*', out.dir = NULL, extract = F) {
  
  ## Setup output folder for image files
  if (is.null(out.dir)) {
    out.dir = paste0(project.dir, '/tmp/')
    if (!dir.exists(out.dir)) { dir.create(out.dir)}
    message('No output directory given, saving to: ', out.dir)
  }
  
  count = 0
  a = Sys.time() # Timer
  
  ## Load each classification file, identify target files, extract if desired
  class.files = list.files(paste0(project.dir, '/R/classification/'), pattern = '.rds', full.names = T)
  
  for (i in 1:length(class.files)) {
    message('Searching for matches in file ', i, ' of ', length(class.files), '... ', appendLF = F)
    
    class = readRDS(class.files[i])
    class = class[grep(class$class, pattern = pattern),]
    
    if (nrow(class) > 0) {
      message('Found \t', nrow(class), ' matches.', appendLF = F)
      count = count + nrow(class)
      
      if (extract) {
        tar.name = paste0(project.dir, '/segmentation/', class$image[1], '.tar.gz')
        
        if (file.exists(tar.name)) {
          class$path = paste0('./segmentation/',
                              class$image,
                              '/', class$image, '_', pad.number(class$frame, 4),
                              '/corrected_crop/',
                              class$image, '_', pad.number(class$frame, 4), '_crop_', pad.number(class$roi), '.png')
          
          #for (k in 1:nrow(class)) {
          #  cmd = paste0('tar -zxvf ', tar.name,' ', class$path[k], ' -C ', out.dir, ' --strip-components=4')
          #  temp = system(cmd, intern = T)
          #}
          archive::archive_extract(archive = tar.name, dir = out.dir, files = class$path)
          message('Copy complete.')
        } else {
          message('No archive file found!')
        }
      } else{
        message()
      }
    } else {
      message('No targets found.')
    }
    
  }
  message('Found ', count, ' valid files (in ', round(as.numeric(difftime(Sys.time(), a, units = 'secs'))), ' seconds).')
}


