CovidWorldAverage <- function(data){
    CovidDataWorld <- data %>%
    mutate(month = month(date)) %>%
    select(location, date, total_cases, month, total_deaths) %>%
    subset(date> "2020-01-01" & date < "2020-12-31") %>%
    na.omit(.) %>% mutate(`Proportion of total cases resulting in deaths`= (total_deaths/total_cases)) %>%
    group_by(location, month) %>%
    summarise_at(vars(`Proportion of total cases resulting in deaths`), ~mean(.)) %>%
    group_by(location) %>% summarise_at(vars(`Proportion of total cases resulting in deaths`), ~mean(.))

    mean(CovidDataWorld$`Proportion of total cases resulting in deaths`)
}