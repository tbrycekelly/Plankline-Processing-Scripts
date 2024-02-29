#' @export
process.classifications = function(base.dir, out.dir) {
  
  #### Classification files
  # In this script, we will load data from each classification file, parse it into a summary data structure, and then save the results to an RDS file
  # in the output directory. This is primarily a preprocessing step. Log files are similarly loaded, parsed, and a summary is saved for the entire 
  # project into the output folder.
  
  class.file = list.files(path = base.dir,
                          pattern = '*_prediction.csv',
                          full.names = T)
  
  
  if (length(class.file) == 0) {
    message('No classifications performed for ', base.dir, '. Skipping!')
    return()
  } else {
    
    for (i in 1:length(class.file)) {
      ## Load file
      class.summary = load.classifications(class.file[i])
      
      ## Save summary file
      saveRDS(class.summary, file = paste0(out.dir, gsub('.csv', '.rds', class.file[i])))
    }
    dev.off()
  }
  return()
}