#' @export

cpuInfo <- function(){
  'lscpu' %>%
    system(intern = TRUE) %>%
    .[!str_detect(.,'Flags:')] %>%
    map(~{
      str_split_fixed(.,':',2) %>%
        set_colnames(c('Type','Value')) %>%
        as_tibble() 
    }) %>%
    bind_rows() %>%
    mutate(Value = str_remove_all(Value,'^[\\s]+'))
}

#' @importFrom parallel detectCores
#' @export

cpus <- function(){
  cpuInfo() %>%
    filter(Type == 'CPU(s)') %>%
    .$Value %>%
    as.numeric()
}