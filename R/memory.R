#' @importFrom magrittr set_colnames
#' @export
memoryInfo <- function(units = 'KB'){
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

#' @export

availableMemory <- function(units = 'KB'){
 memoryInfo(units = units) %>%
    filter(Type == 'MemAvailable') %>%
    .$Size
}

#' @importFrom stringr str_remove_all
#' @export

totalMemory <- function(units = 'KB'){
  memoryInfo(units = units) %>%
    filter(Type == 'MemTotal') %>%
    .$Size
}

#' @export

usedMemory <- function(units = 'KB'){
  totalMemory(units = units) - availableMemory(units = units)
}