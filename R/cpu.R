#' cpuInfo
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

#' cpus
#' @description Return number of system cpus.
#' @importFrom parallel detectCores
#' @export

cpus <- function(){
  cpuInfo() %>%
    filter(Type == 'CPU(s)') %>%
    .$Value %>%
    as.numeric()
}

#' cpuUsage
#' @description Total system cpu load.
#' @export

cpuLoad <- function(){
  processes() %>%
   .$`%CPU` %>%
    as.numeric() %>%
    {. / 100} %>%
    sum()
}

#' cpuUser
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