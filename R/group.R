#' @export
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