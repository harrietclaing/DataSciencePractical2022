LondonPlot <- function(data) {


     Londondf <- data %>%
        subset(date>20010101) %>%
        mutate(formatyear=ymd(date)) %>%
        mutate(Year=year(formatyear)) %>%
        group_by(Year) %>%
        summarise_at(vars(precipitation), ~sum(., na.rm=T)) %>%
        select(Year, precipitation)

    LondonRainPlot <- Londondf %>%
        ggplot() +
        geom_line(aes(x=Year, y=precipitation), group=1, size=0.6, color='blue') +
        geom_hline(yintercept = 469.9, color='red') +
        ylab("Annual precipition (mm)") +
        labs(subtitle= "Source: Climate Knowledge Portal, World Bank", title="Annual precipitation in London in the last ten years compared with the average annual precipitation for South Africa") +
        theme_light()

    LondonRainPlot

}