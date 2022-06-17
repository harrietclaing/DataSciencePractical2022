NetflixDataRuntimePlot <- function(data1, data2){

    NetflixData <- left_join(data1, data2, by='id') %>% as_tibble(.) %>% select(role, name, imdb_score, type, release_year, runtime, genres, production_countries)

    NetflixDataRuntime <- NetflixData %>% group_by(runtime) %>% summarise_at(vars(imdb_score), ~mean(., na.rm=T))

    NetflixDataRuntime %>% ggplot() + geom_line(aes(x=runtime, y=imdb_score), group=1, color='pink', size=1.6) +theme_minimal() + labs(title="Uncertain relationship between IMDB score and run time of Netflix movies & series")

}