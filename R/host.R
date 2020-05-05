#' hostname
#' @description Return hostname.
#' @export

hostname <- function(){
  system('hostname',intern = TRUE)
}

#' ipaddress
#' @description Return the IPv4 address.
#' @export

ipaddress <- function(){
 system('ip addr',intern = TRUE) %>%
    {
      .[str_detect(.,'inet') | str_detect(.,'<BROADCAST')]
    } %>%
    {
      .[which(str_detect(.,'<BROADCAST')) + 1]
    } %>%
    stringr::str_split_fixed('/',2) %>%
    .[,1] %>%
    str_remove_all('inet') %>%
    str_remove_all(' ')
}
