
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




load.measurements.old = function(file.path, verbose = T) {
  ## Determine files in the given archive. Find the csv file
  files = as.data.frame(archive::archive(file.path))
  l = which(grepl(files$path, pattern = '.csv'))
  
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

load.measurements = function(file.path, verbose = T) {
  
  ## Get measurement file name
  f = strsplit(file.path, '/')[[1]]
  f = gsub(pattern = '.tar.gz', replacement = '.csv', f[length(f)])
  f = paste0('./measurements/', f)
  
  ## Load the csv file from inside the archive
  data = read.csv(archive::archive_read(archive = file.path, file = f))
  
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



pull = function(project.dir, p = 0.5, out.dir = NULL, scratch = '/tmp/pull') {
  
  ## Setup output folder for image files
  if (is.null(out.dir)) {
    out.dir = paste0(project.dir, '/tmp/')
    if (!dir.exists(out.dir)) { dir.create(out.dir)}
    message('No output directory given, saving to: ', out.dir)
  }
  
  if (!dir.exists(scratch)) {
    message('Creating scartch directory.')
    dir.create(scratch)
  } else {
    stop('Scratch directory exists, must not exist for data protection! Dir = ', scratch)
  }
  
  
  count = 0
  a = Sys.time() # Timer
  
  ## Load each classification file, identify target files, extract if desired
  class.files = list.files(paste0(project.dir, '/classification/'), pattern = '.csv', full.names = T)
  
  for (i in 1:length(class.files)) {
    message('Reading csv ', i, ' of ', length(class.files), '...')
    data = fread(class.files[i])
    
    message('Extracting TAR file to temporary directory...')
    tar.file = gsub('.csv', '.tar', gsub('classification', 'segmentation', class.files[i]))
    tar.file = strsplit(tar.file, '-')[[1]]
    tar.file = paste0(paste0(tar.file[1:(length(tar.file)-3)], collapse = '-'), '.tar')
    
    if (file.exists(tar.file)) {
      system(paste0('tar -xf ', tar.file, ' -C ', scratch, ' --strip-components=4 --wildcards "*.png"'))
    } else {
      stop('No matching Tar file found!')
    }
    
    paths = strsplit(data$image, '/')
    for (taxa in colnames(data)[-1]) {
      l = which(data[[taxa]] > p)
      
      if (length(l) > 0) {
        if (!dir.exists(paste0(out.dir, '/', taxa))) {
          dir.create(paste0(out.dir, '/', taxa))
        }
        
        for (k in l) {
          count = count + 1
          file.copy(from = paste0(scratch, '/', paths[[k]][length(paths[[k]])]), to = paste0(out.dir, '/', taxa))
          file.remove(paste0(scratch, '/', paths[[k]][length(paths[[k]])]))
        }
      }
    }
    
    ## Copy over whatever files are left (unclassified objects)
    if (!dir.exists(paste0(out.dir, '/_unsorted/'))) {dir.create(paste0(out.dir, '/_unsorted/'))}
    orphan = list.files(scratch, pattern = '*.png', full.names = T)
    file.copy(from = orphan, to = gsub(scratch, paste0(out.dir, '/_unsorted/'), orphan))
    
    file.remove(list.files(scratch, pattern = '*', full.names = T))
  }
  
  file.remove(scratch)
  
  
  ## Make morphocluster index
  images = list.files(path = out.dir, recursive = T, pattern = '*.png', full.names = F)
  name = strsplit(images, split = '/')
  index = data.frame(object_id = NA, path = images)
  
  for (i in 1:nrow(index)) {
    index$object_id[i] = gsub('.png', '', name[[i]][length(name[[i]])])
  }
  
  write.csv(index, file = paste0(out.dir, '/index.csv'), row.names = F)
  
  
  ## Done
  message('Found ', count, ' valid files (in ', round(as.numeric(difftime(Sys.time(), a, units = 'secs'))), ' seconds).')
}


export.morpho = function() {
  
  ## Remove and create export file
  message('To make the zip file: zip -FSr ../export.zip ./*')
  
  ## Copy to container
  message('to Copy data to container: sudo docker cp /media/plankline/Data/Data/osu_test/export.zip morphocluster2_morphocluster_1:/data')
  
  ## activate docker
  message('Log in: sudo docker-compose exec morphocluster bash')
  message('First: . ./activate; cd /data')
  
  ## Run feature extraction
  
}



