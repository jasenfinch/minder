#' System CPU information
#' @description Return system cpu information.
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate
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

#' The number of system CPUs
#' @description Return number of system CPUs.
#' @importFrom parallel detectCores
#' @export

cpus <- function(){
  cpuInfo() %>%
    filter(Type == 'CPU(s)') %>%
    .$Value %>%
    as.numeric()
}

#' Current system CPU usage
#' @description Total system cpu load.
#' @export

cpuLoad <- function(){
  processes() %>%
   .$`%CPU` %>%
    as.numeric() %>%
    {. / 100} %>%
    sum()
}

#' System CPU usage by user
#' @description Return user cpu usage.
#' @importFrom dplyr arrange desc group_by summarise
#' @export

cpuUser <- function(){
  processes() %>%
    mutate(CPUs = as.numeric(`%CPU`)) %>%
    group_by(USER) %>%
    summarise(CPUs = sum(CPUs) %>% {. / 100},`%` = CPUs / cpus() * 100) %>%
    filter(CPUs > 0) %>%
    arrange(desc(CPUs))
}