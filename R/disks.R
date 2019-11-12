#' partitions
#' @description Summary of system disk partitions.
#' @param units units in which to return memory values (B,KB,MB,GB)
#' @importFrom dplyr rename
#' @export

partitions <- function(units = 'GB'){
  dsks <- system('df',intern = TRUE) %>%
    map(~{
      str_split_fixed(.,pattern = '\\s+',n = 6) %>%
        {suppressMessages(as_tibble(.,.name_repair = 'universal'))}
    }) %>%
      bind_rows() %>%
    set_colnames(.[1,]) %>%
    filter(Filesystem != 'Filesystem') %>%
    mutate(`1K-blocks` = as.numeric(`1K-blocks`),
           Used = as.numeric(Used),
           Available = as.numeric(Available)) %>%
    rename(Size = `1K-blocks`)
  
  if (units != 'KB') {
    dsks <- dsks %>%
      mutate(Size = convertUnits(Size,'KB',units),
             Used = convertUnits(Used,'KB',units),
             Available = convertUnits(Available,'KB',units))
  }
  return(dsks)
}