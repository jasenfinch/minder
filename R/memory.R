#' memory
#' @description Return system memory usage information.
#' @param units units in which to return memory values (B,KB,MB,GB)
#' @importFrom magrittr set_colnames
#' @export
memoryInfo <- function(units = 'GB'){
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
             )
  if (units != 'B') {
    mem <- mem %>%
      mutate(Size = convertUnits(Size,'B',units))
  }
  return(mem)
}

#' convertUnits
#' @description Convert between memory units.
#' @param value value to convert
#' @param from units converting from
#' @param to units to convert to
#' @param factor conversion factor
#' @details Units can include B, KB, MB, GB
#' @importFrom tibble tibble
#' @export

convertUnits <- function(value,from,to,factor = 1024){
  convert <- tibble(Unit = c('B','KB','MB','GB'),Value = c(0:3)) %>%
    filter(Unit == from | Unit == to)
  
  from <- convert$Value[convert$Unit == from]
  to <- convert$Value[convert$Unit == to]
  
  if (from < to) {
    value / factor ^ (to - from)
  } else {
    value * factor ^ (from - to)
  }
}

#' memoryAvailable
#' @description Return available system memeory.
#' @param units units in which to return memory values (B,KB,MB,GB) 
#' @export

memoryAvailable <- function(units = 'GB'){
 memoryInfo(units = units) %>%
    filter(Type == 'MemAvailable') %>%
    .$Size
}

#' memoryTotal
#' @description Return total system memeory.
#' @param units units in which to return memory values (B,KB,MB,GB) 
#' @importFrom stringr str_remove_all
#' @export

memoryTotal <- function(units = 'GB'){
  memoryInfo(units = units) %>%
    filter(Type == 'MemTotal') %>%
    .$Size
}

#' usedMemory
#' @description Return used system memeory.
#' @param units units in which to return memory values (B,KB,MB,GB) 
#' @export

memoryUsed <- function(units = 'GB'){
  memoryTotal(units = units) - memoryAvailable(units = units)
}

#' userMemory
#' @description Return user system memory usage.
#' @param units units in which to return memory values (B,KB,MB,GB)
#' @importFrom dplyr select
#' @export

memoryUser <- function(units = 'GB'){
  procs <- processes() %>%
    mutate(MEM = RSS %>%
             str_remove_all('[:alpha:]') %>%
             as.numeric()
    ) %>%
    select(USER,MEM)
  
  if (units != 'KB') {
    procs <- procs %>%
      mutate(MEM = convertUnits(MEM,'KB',units))
  }
  
  mem <- procs %>%
    group_by(USER) %>%
    summarise(MEM = sum(MEM)) %>%
    arrange(desc(MEM)) %>%
    filter(MEM > 0) %>%
    mutate(`%MEM` = MEM / memoryTotal(units) * 100) %>%
    select(USER,MEM,`%MEM`)
  
  return(mem)
}