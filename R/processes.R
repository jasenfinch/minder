#' processes
#' @description Return system processes
#' @param root should root processes be returned
#' @examples 
#' processes()
#' @importFrom magrittr %>%
#' @importFrom stringr str_split_fixed
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows filter
#' @importFrom purrr map
#' @export

processes <- function(root = TRUE){
  a <- system('ps -aux',intern = T) %>%
    map(~{
      str_split_fixed(.,pattern = '\\s+',n = 11) %>%
        as_tibble()
    }) %>%
    bind_rows()
  
  colnames(a) <- a[1,]
  a <- a[-1,]
  
  if (isFALSE(root)) {
    a %>%
      filter(USER != 'root')
  }
  return(a)
}

