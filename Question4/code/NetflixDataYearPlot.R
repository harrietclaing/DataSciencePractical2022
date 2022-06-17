NetflixDataYearPlot <- function(data1, data2){

    NetflixData <- left_join(data1, data2, by='id') %>% as_tibble(.) %>% select(imdb_score, type, release_year, runtime, genres, production_countries)

    NetflixDataYear <- NetflixData %>% group_by(release_year) %>% summarise_at(vars(imdb_score), ~mean(.))

    NetflixDataYearPlot <- NetflixDataYear %>% ggplot() + geom_col(aes(x=release_year, y=imdb_score), stat='identity', na.rm=T, fill='green') + theme_minimal() + coord_cartesian(ylim=c(5,8.5)) + labs(title="Average IMDB ratings for movies released in years from 1950s to 2000s")

    NetflixDataYearPlot

}