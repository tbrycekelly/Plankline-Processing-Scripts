#' @export
load.acs.files = function(files, dt = 1, min.size = 1024, verboes = T) {
  
  #### Filter files by size
  file = data.frame(files = files, keep = T)
  
  ## Remove files smaller than 1kB
  for (i in 1:nrow(file)) {
    if (file.size(file$file[i]) < min.size) {
      file$keep[i] = F
    }
  }
  
  message(' Identified ', sum(file$keep), ' file(s) of valid size and ', sum(!file$keep), ' file(s) of invalid size.')
  files = file$files[file$keep]
  
  ## Load in first file.
  message(' Attempting to load file 1... ', appendLF = F)
  start.time = file.info(files[1])$ctime ## inital guess
  dat = as.data.frame(fread(files[1], skip = 99, verbose = F, showProgress = F))
  dat$Time = dat$`Time(ms)`/1000 + start.time - max(dat$`Time(ms)`/1000)
  dat$`Time(ms)` = NULL
  message('Done.')
  
  ## Read in rest of data
  for (i in 2:length(files)) {
    message(' Attempting to load file ', i, '... ', appendLF = F)
    temp = as.data.frame(fread(files[i], skip = 99, verbose = F, showProgress = F))
    
    start.time = file.info(files[i])$ctime ## inital guess
    temp$Time = temp$`Time(ms)`/1000 + start.time - max(temp$`Time(ms)`/1000)
    temp$`Time(ms)` = NULL
    
    if (ncol(temp) != ncol(dat)) {
      message('Invalid number of columns (', ncol(temp), ' vs ', ncol(dat), '), skipping.')
    } else {
      dat = rbind.data.frame(dat, temp)
      message('Done.')
    }
  }
  
  message('Finished.')
  dat
}
