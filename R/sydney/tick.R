tick_to_posix <- function(x, tz = "UTC", ...) {
  
  x <- as.numeric(as.character(x)) / 10000000
  
  as.POSIXct(x, tz, origin = "0001-01-01", ...)
  
}
