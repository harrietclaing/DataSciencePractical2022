CovidAfricaPlot <- function(data){
    CovidAfricaPlotdf <- data %>%
        mutate(month = month(date)) %>%
        select(continent, location, date, new_deaths_per_million, month) %>%
        subset(date> "2020-01-01" & date < "2020-12-31") %>%
        na.omit(.) %>%
        group_by(continent, month) %>%
        summarise_at(vars(new_deaths_per_million), ~mean(.))

    CovidAfricaPlot <- CovidAfricaPlotdf %>%
        ggplot() +
        geom_col(aes(x=month, y=new_deaths_per_million, fill=continent)) +
        labs(title="Evolution of Covid outbreak of new monthly deaths in 2020") +
        ylab("Average new deaths (per million)") +
        xlab("Months") +
        scale_x_discrete(labels=c('January', 'February'))

    CovidAfricaPlot
}