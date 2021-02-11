#' System memory information
#' @description Return system memory usage information.
#' @importFrom magrittr set_colnames
#' @importFrom fs as_fs_bytes
#' @export
memoryInfo <- function(){
  mem <- 'cat /proc/meminfo' %>%
    system(intern = TRUE) %>%
    map(~{
      str_split_fixed(.,':',2) %>%
        set_colnames(c('Type','Size')) %>%
        as_tibble() 
      }) %>%
    bind_rows() %>%
    mutate(Size = str_remove_all(Size,'\\s'),
           Size = str_remove_all(Size,'[:alpha:]') %>% 
             as.numeric() %>%
             {. * 1024}
             ) %>%
    mutate(Size = as_fs_bytes(Size))
  return(mem)
}

#' Available system memory
#' @description Return available system memeory.
#' @export

memoryAvailable <- function(){
 memoryInfo() %>%
    filter(Type == 'MemAvailable') %>%
    .$Size
}

#' Total system memory
#' @description Return total system memeory.
#' @export

memoryTotal <- function(){
  memoryInfo() %>%
    filter(Type == 'MemTotal') %>%
    .$Size
}

#' Used system memory
#' @description Return used system memeory.
#' @export

memoryUsed <- function(){
  memoryTotal() - memoryAvailable()
}

#' System memory by user
#' @description Return user system memory usage.
#' @importFrom dplyr select
#' @importFrom stringr str_remove_all
#' @export

memoryUser <- function(){
  procs <- processes() %>%
    mutate(MEM = RSS %>%
             str_remove_all('[:alpha:]') %>%
             as.numeric() %>%
             {. * 1024} %>%
             as_fs_bytes()
    ) %>%
    select(USER,MEM)
  
  mem <- procs %>%
    group_by(USER) %>%
    summarise(MEM = sum(MEM)) %>%
    arrange(desc(MEM)) %>%
    filter(MEM > 0) %>%
    mutate(`%MEM` = {MEM / memoryTotal() * 100} %>%
             as.numeric()) %>%
    select(USER,MEM,`%MEM`)
  
  return(mem)
}