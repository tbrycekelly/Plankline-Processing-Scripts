library(PlanklinePS)


#### Project preparation and parameter setting
## Setup
root.dir = '/media/plankline/Data/Data/'
frame.rate = 20.5
bin.width = 2 # second(s)
p.threshold = 0.5 # probability threshold

dirs = list.dirs(root.dir, recursive = F, full.names = T)
dirs = dirs[!grepl('test', dirs)]
#dirs = dirs[grepl('Done', dirs)]


for (base.dir in dirs) {
  
  message(Sys.time(), ': Starting to process ', base.dir)
  
  if (dir.exists(paste0(base.dir, '/segmentation')) & dir.exists(paste0(base.dir, '/classification'))) {
  
  out.dir = paste0(base.dir, '/R/')
  if (!dir.exists(out.dir)) { dir.create(out.dir)}
  
  process.classifications(base.dir = base.dir,
                          out.dir = out.dir)
  
  process.measurements(base.dir = base.dir,
                       out.dir = out.dir)
  
  generate.bin(out.dir = out.dir,
               p.threshold = p.threshold,
               frame.rate = frame.rate,
               bin.width = bin.width)
  
  } else {
    message('Missing directories in ', base.dir,', skipping preprocessing.')
  }
}
