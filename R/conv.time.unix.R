
#' @export
conv.time.unix = function(x, tz = 'GMT') {
  #if (any(is.POSIXct(x))) { return(x) }
  as.POSIXct(x, origin = "1970-01-01", tz = tz)
}