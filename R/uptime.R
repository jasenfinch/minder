#' uptime
#' @description Return system uptime in seconds
#' @importFrom lubridate ymd_hms now time_length
#' @export

uptime <- function(quiet = FALSE){
  since <- system('uptime -s',intern = TRUE)  %>% 
    ymd_hms(tz = Sys.timezone())
  
  {now(tzone = Sys.timezone()) - since} %>%
    time_length()
}