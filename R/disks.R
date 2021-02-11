#' Disk partition information
#' @description Summary of system disk partitions.
#' @importFrom dplyr rename mutate_if
#' @export

partitions <- function(){
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
    rename(Size = `1K-blocks`) %>%
    mutate_if(is.numeric,~{. * 1024}) %>%
    mutate_if(is.numeric,as_fs_bytes)
  
  return(dsks)
}