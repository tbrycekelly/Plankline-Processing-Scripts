#' @title Load Classifications File (Plankline)
#' @export
load.classifications = function(file, verbose = T) {
  
  if (verbose) {message('Attempting to read file ', file)}
  class.data = data.table::fread(file)
  
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