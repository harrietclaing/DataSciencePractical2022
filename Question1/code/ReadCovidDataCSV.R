ReadCovidDataCSV <- function(dataroot){
    library(readr)
    owid_covid_data <- read_csv(dataroot)
}