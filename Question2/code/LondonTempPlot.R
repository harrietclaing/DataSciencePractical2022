LondonTempPlot <- function(data){

    LondonTempdf <- data %>%
        subset(date>19790101) %>%
        mutate(formatyear=ymd(date)) %>%
        mutate(Year=year(formatyear)) %>%
        group_by(Year) %>%
        summarise_at(vars(mean_temp), ~mean(., na.rm=T)) %>%
        select(Year, mean_temp)

LondonTempPlot <- LondonTempdf %>%
    ggplot() +
    geom_line(aes(x=Year, y=mean_temp), group=1, size=0.6, color='blue') +
    geom_hline(yintercept = 17.5, color='red') +
    ylab("Annual average temperature") +
    labs(subtitle= "Source: Climate Knowledge Portal, World Bank", title="Average annual temperatures for London since 1970 with comparison to South Africa's average annual temperature") +
    theme_light()

LondonTempPlot
}