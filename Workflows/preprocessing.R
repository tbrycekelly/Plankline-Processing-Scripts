library(PlanklinePS)

args = commandArgs(trailingOnly = TRUE)

## parameters
frame.rate = 20.5
bin.width = as.numeric(args[1]) # second(s)
p.threshold = as.numeric(args[2]) # probability threshold

## File names
tmp = strsplit(x = args[3], split = '/')[[1]]
file = tmp[length(tmp)]
path = gsub(file, '', args[3])
base.dir = gsub('/classification/', '', path)

measure.file = paste0(base.dir, '/segmentation/', gsub('.csv', '.tar', file))


out.dir = paste0(base.dir, '/R/')
if (!dir.exists(out.dir)) {
  dir.create(out.dir)
  dir.create(paste0(out.dir, '/classification/'))
  dir.create(paste0(out.dir, '/measurement/'))
  dir.create(paste0(out.dir, '/bin/'))
}

class = load.classifications(args[3])
saveRDS(class, file = paste0(out.dir, '/classification/', class$image[1], ' classification.rds'))

summary = load.measurements(measure.file)
saveRDS(summary, file = paste0(out.dir, '/measurement/', summary$image[1], ' measurement.rds'))


## Add timestamp
temp = strsplit(class$image[1], split = '-')[[1]]
class$time = as.POSIXct(paste0(temp[3], '-', temp[4], '-', temp[5], ' ', temp[6], ':', temp[7], ':', temp[8]), tz = 'UTC') + (class$frame - 1) / frame.rate

## bin
class$time = as.POSIXct(round(as.numeric(class$time) / bin.width) * bin.width, origin = '1970-01-01')
bin = data.frame(time = unique(class$time))

bin$frames = sapply(bin$time, function(x) {diff(range(class$frame[class$time == x]))})

for (n in unique(class$class)) { ## Already normalized to number of frames!
  bin[[n]] = sapply(bin$time, function(x) {sum(class$time == x & class$class == n & class$p >= p.threshold)}) / bin$frames 
}

## output
saveRDS(bin, file = paste0(out.dir, '/bin/', class$image[1], ' bin.rds'))