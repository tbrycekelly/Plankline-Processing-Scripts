#' @export
process.classifications = function(base.dir, out.dir) {

  #### Classification files
  # In this script, we will load data from each classification file, parse it into a summary data structure, and then save the results to an RDS file
  # in the output directory. This is primarily a preprocessing step. Log files are similarly loaded, parsed, and a summary is saved for the entire 
  # project into the output folder.
  
  class.file = list.files(path = paste0(base.dir, '/classification/'),
                          pattern = '*.csv',
                          full.names = T)
  
  
  if (length(class.file) == 0) {
    message('No classifications performed for ', base.dir, '. Skipping!')
    return()
  } else {
  
    ## Preprocess the classification files here:
    if (!dir.exists(paste0(out.dir, '/classification/'))) {
      dir.create(paste0(out.dir, '/classification/'))
    }
    
    pdf(paste0(out.dir, '/Classification Probability Density.pdf'))
    par(mfrow = c(2,2))
    
    for (i in 1:length(class.file)) {
      ## Load file
      class.summary = load.classifications(class.file[i])
        
      ## Save summary file
      saveRDS(class.summary, file = paste0(out.dir, '/classification/', class.summary$image[1], ' classification.rds'))
      
      plot(density(class.summary$p),
            xlim = c(0, 1),
            ylim = c(0,2.5),
            yaxs = 'i',
            xaxs = 'i',
            lwd = 3,
            main = class.summary$image[1])
    }
    dev.off()
  }
  return()
}