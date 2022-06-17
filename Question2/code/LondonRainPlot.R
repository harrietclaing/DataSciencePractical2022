LondonRainPlot <- function(dataroot, data=LondonData){
    library(readr)
    Londondata <- read.csv(dataroot)

    LondonRaindf <- Londondata %>% subset(date>20010101) %>% mutate(formatyear=ymd(date)) %>% mutate(Year=year(formatyear)) %>% group_by(Year) %>% summarise_at(vars(precipitation), ~sum(., na.rm=T)) %>% select(Year, precipitation)

    LondonRainPlot <- LondonRaindf %>% ggplot() + geom_line(aes(x=Year, y=precipitation), group=1, size=0.6, color='blue') + geom_hline(yintercept = 469.9, color='red') + ylab("Annual precipiation (mm)") + labs(title="Annual precipitation in London in the last ten years compared with the average annual precipitation for South Africa") + theme_light()

    LondonRainPlot

}