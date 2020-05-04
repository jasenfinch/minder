#' whoami
#' @description Return the user name associated with the current effective user ID.
#' @export

whoami <- function(){
  system('whoami',intern = TRUE)
}