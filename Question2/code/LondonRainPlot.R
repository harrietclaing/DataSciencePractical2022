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
        geom_line(aes(x=Year, y=precipitation), group=1, size=1.3, alpha=0.8, color='blue') +
        geom_hline(yintercept = 469.9, color='red', size=0.9) +
        ylab("Annual precipition (mm)") +
        labs(subtitle= "Source: Climate Knowledge Portal, World Bank", title="Annual precipitation in London in the last ten years compared with the average annual precipitation for South Africa") +
        theme_light() +
        geom_text(aes(max(Year), 469.9, label = "South Africa Avg. 469.9 mm", vjust = - 1), col = "red") +
        theme_minimal()

    LondonRainPlot

}