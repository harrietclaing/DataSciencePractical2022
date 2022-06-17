CovidSmokingPlot <- function(data){

    CovidSmokingdf <- data %>%
        subset(location =="Nauru"|location =="Kiribati"|location =="Tuvalu"|location == "Myanmar"|location =="Chile"|location =="Lebanon"|location =="Serbia"|location == "Bangladesh"| location=="Greece"|location =="Bulgaria") %>%
        mutate(month = month(date)) %>%
        select(location, date, total_cases, month, total_deaths) %>%
        subset(date> "2020-01-01" & date < "2020-12-31") %>%
        na.omit(.) %>%
        mutate(`Proportion of total cases resulting in deaths`= (total_deaths/total_cases)) %>%
        group_by(location) %>%
        summarise_at(vars(`Proportion of total cases resulting in deaths`), ~mean(.))

    CovidSmokingPlot <- CovidSmokingdf %>%
        ggplot() +
        geom_col(aes(x=location, y=`Proportion of total cases resulting in deaths`), fill='orange')  +
        theme_minimal() +
        labs(title="Top 10 smoking countries", subtitle="Investigating Covid severity by looking at proportion of total cases that resulted in deaths compared to the world average during 2020") +
        theme(axis.text.x = element_text(color="#993333", size=8, angle=45)) +
        xlab("Countries")

    CovidSmokingPlot
}