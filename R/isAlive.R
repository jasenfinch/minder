#' Check if a host is active
#' @description Check if a host is active.
#' @param host host address
#' @param port port to check
#' @importFrom pingr ping ping_port
#' @examples isAlive('www.google.com')
#' @export

isAlive <- function(host,port = NULL){
  if (is.null(port)) {
    response <- ping(host,count = 1)
  } else {
    response <- ping_port(host,port = port)
  }
  
  if (is.na(response)) {
    alive <- FALSE
  } else {
    alive <- TRUE
  }
  
  return(alive)
}