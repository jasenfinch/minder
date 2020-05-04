#' uptime
#' @description Return system uptime in seconds
#' @param unit a character string that specifies with time units to use (see \code{?lubridate::time_length})
#' @importFrom lubridate ymd_hms now time_length interval
#' @export

uptime <- function(unit = 'second'){
  since <- system('uptime -s',intern = TRUE)  %>% 
    ymd_hms(tz = Sys.timezone())
  
  interval(since,now(tzone = Sys.timezone())) %>%
    time_length(unit = unit)
}