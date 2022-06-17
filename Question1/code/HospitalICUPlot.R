HospitalICUPlot <- function(data=CovidData, Continent){
    HospitalICUdf <- data %>% subset(continent == Continent) %>% group_by(continent, date) %>%
        summarise_at(vars(weekly_hosp_admissions_per_million,
                          weekly_icu_admissions_per_million), ~mean(., na.rm=T)) %>% gather(`Admission type`,`Weekly admissions per million`, 3:4, na.rm=T)

    HospitalICUdf %>% ggplot() + geom_line(aes(x=date, y=`Weekly admissions per million`, color= `Admission type`))

}