---
output:
  md_document:
    variant: html_document
---

# Data Science Practical 2022

## Setting up the Question folders

I set up each question in Texevier and put each question's related data within its separate folder.

```{r}

rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
library(tidyverse)
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

```{r}
CHOSEN_LOCATION <- "C:/Users/Harriet/Documents/DataSciencePractical2022"
fmxdat::make_project(FilePath = glue::glue("{CHOSEN_LOCATION}"), 
                     ProjNam = "21617023")

Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}"), template_name = "Question1")
Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}"), template_name = "Question2")
Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}"), template_name = "Question3")
Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}"), template_name = "Question4")
```

# Question 1: Evolution of Covid outbreak
```{r}
library(tidyverse)
library(lubridate)
library(readr)
library(gridExtra)
list.files('Question1/code/', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
```


First, we create a function to read in our data and source the code.
```{r}
source("Question1/code/ReadCovidDataCSV.R")

CovidData <- ReadCovidDataCSV(dataroot="Question1/data/Covid/owid-covid-data.csv")

```
In the Our World in Data folder, we have data on 6 continents and many rows with values of N/A. We have approximately 800 time periods, but does not appear to be consistent. Therefore, we restrict our period of interest to 2020 and create values per month from January 2020 and removed the N/As for our functions.

## Figure 1: Investigating the misconception of the experience of African countries with Covid compared with other regions.

This graph shows that Africa did not experience Covid as badly compared with other countries, in terms of new deaths on average per continent.

```{r}
source("Question1/code/CovidAfricaPlot.R")

CovidAfricaPlot(CovidData)

/Question1/CovidAfricaPlot.png
```

## Figure 2: Countries with high prevalence of smoking & severity of Covid

According to World Population Review, the countries that have the highest prevalence of smoking are:

Nauru (52.10%)
Kiribati (52.00%)
Tuvalu (48.70%)
Myanmar (45.50%)
Chile (44.70%)
Lebanon (42.60%)
Serbia (40.60%)
Bangladesh (39.10%)
Greece (39.10%)
Bulgaria (38.90%)

Compare these countries with highest prevalence of smoking to the world average of new deaths in 2020 from Covid. I subsetted the data to obtain a data set with only these countries and compare total cases and total deaths to see if they had a higher proportion and people dying from Covid as the numbers of infected persons rose. 
```{r}
source("Question1/code/CovidSmokingPlot.R")

CovidSmokingPlot <- CovidSmokingPlot(CovidData)

source("Question1/code/CovidWorldAverage.R")

CovidWorldAverage <- CovidWorldAverage(CovidData)

CovidSmokingPlotFinal <- CovidSmokingPlot + geom_hline(yintercept = 0.034218, color='blue', size=1.5) + geom_text(aes(max(location), 0.034218,label = "Calculated world average (2020)", vjust = - 1), col = "blue")
```
```{r}
CovidSmokingPlotFinal
```
We find that the country's average for proportion of total cases resulting in deaths from the data set provided is 0.034218 averaged across 2020. Add in this comparison to the graph.

## Figure 3: How quickly did different regions increase hospitalisation facilities? 

Did that time series of hospital beds lead or lag on ICU admission rates? By removing the na values, we subset the data to begin at March 8th. Omitted Oceania because lacking data available.

```{r}

source('Question1/code/HospitalICUPlot.R')


AsiaHospitalICUPlot <- HospitalICUPlot(Continent="Asia") + labs("Asia weekly hospital admissions and weekly ICU admissions")
AsiaHospitalICUPlot

AfricaHospitalICUPlot <- HospitalICUPlot(data=CovidData, Continent="Africa") + labs("Africa weekly hospital admissions and weekly ICU admissions")
AfricaHospitalICUPlot

EuropeHospitalICUPlot <- HospitalICUPlot(data=CovidData, Continent="Europe") + labs("Europe weekly hospital admissions and weekly ICU admissions")
EuropeHospitalICUPlot

NorthAmericaHospitalICUPlot <- HospitalICUPlot(data=CovidData, Continent="North America") + labs("North America weekly hospital admissions and weekly ICU admissions")
NorthAmericaHospitalICUPlot

SouthAmericaHospitalICUPlot <- HospitalICUPlot(data=CovidData, Continent="South America") + labs("South America weekly hospital admissions and weekly ICU admissions")
SouthAmericaHospitalICUPlot
```

# Question 2: London weather

```{r}
source("Question2/code/ReadLondonDataCSV.R")

LondonData <- ReadLondonDataCSV("Question2/data/London/london_weather.csv")
```
First, subset to last 10 years. Data goes up to 31/12/2021, therefore subset from 2001. 
```{r}
library(lubridate)

```

Second, we want to try and show the rainfall over time. We want annual rainfall per year for the last ten years.
```{r}
library(readr)

source('Question2/code/LondonRainPlot.R')

LondonRainPlot <- LondonPlot(data= LondonData) 

LondonRainPlot   
```

Third, we want to try and show average temperatures over time since 1970.
```{r}

source('Question2/code/LondonTempPlot.R')

LondonTempPlot(data=LondonData)
```

# Question 3: Evolution of tennis players' performance over time

First, write a function to read-in multiple csv sheets for rankings data

```{r}
Data_Collating <- function(Datroot){

library(tidyverse)

    # let's create a silent read function first (as it prints a load of nonsense if you use read_csv directly):
    silentread <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result
    }

    datcolat <-
        list.files(Datroot, full.names = T, recursive = T) %>%
        # Ensure you only load the csv's, not the README.txt.
        .[grepl("atp_matches", .)] %>%
        as.list() %>%
        map(~silentread(.)) %>% bind_rows()
        # equivalent to using map_df

    datcolat

}

TennisDataMatches <- Data_Collating("Question3/data/Tennis")
tail(TennisDataMatches)
#ONLY 2000S DATA
```

```{r}
Data_Collating2 <- function(Datroot){

library(tidyverse)

    # let's create a silent read function first (as it prints a load of nonsense if you use read_csv directly):
    silentread <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result
    }

    datcolat <-
        list.files(Datroot, full.names = T, recursive = T) %>%
        # Ensure you only load the csv's, not the README.txt.
        .[grepl("atp_rankings_*.csv", .)] %>%
        as.list() %>%
        map(~silentread(.)) %>% bind_rows()
        # equivalent to using map_df

    datcolat

}

TennisDataRankings <- Data_Collating2("Question3/data/Tennis")
```

```{r}
TennisData <- left_join(TennisDataMatches, TennisDataRankings, by=)
```


```{r}
library(tidyverse)
library(readr)
library(vroom)

files <- list.files("Question3/data/Tennis", full.names = T, recursive = T) %>%
            .[grepl("atp_matches_20*.csv", .)] %>%
            map_df(~read.csv(.)) 
class(files)
as.vector(files)

data <- vroom(files, col_select = c(!tourneyid, winner_name, winner_hand, winner_age, winner_ht, winner_ioc, winner_id, tourney_date), skip = 1)
                  
                  
files <- fs::dir_ls(glob="atp_matches_20*.csv")
files
vroom(files)

vroom()
```

```{r}
ReadinMultipleCSV <- function(dataroot){
        df <- list.files(dataroot, full.names = T, recursive = T) %>%
            .[grepl(".csv", .)] %>%
            map_df(~read.csv(.)) 
        
    df
}  

df <- list.files("Question3/data/Tennis" pattern = ".csv") %>% 
    map_df(~read_csv(.))

 df <- list.files("Question3/data/Tennis/", full.names = T, recursive = T) %>%
            grepl(".csv") %>%
            as.list(.) %>% 
            map_df(~read.csv(.))
 
datcolat <- list.files("Question3/data/Tennis/") %>%
        # Ensure you only load the csv's, not the README.txt.
        .[grepl(".csv", .)] %>%
        as.list() %>%
        map(~read.csv(.)) %>% bind_rows()
        # equivalent to using map_df

    datcolat
            

TennisData <- ReadinMultipleCSV("Question3/data/Tennis")
    
    %>%
        map(~silentread(.)) %>% bind_rows()
        # equivalent to using map_df

Data_Collating <- function(Datroot){

library(tidyverse)
    library(dplyr)
    library(vroom)

    # let's create a silent read function first (as it prints a load of nonsense if you use read_csv directly):
    silentread <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result
    }
    
    
files <- list.files("Question3/data/Tennis", full.names = T, recursive = T, row.names=F) %>% .[grepl("atp_matches",.)] %>%
        as.list() %>%
        map(~read.csv(.)) %>% bind_rows()
?vroom

files <- fs::dir_ls(path= "Question3/data/Tennis", glob = "atp_matches-.csv")

data <- vroom(files, col_select = c(winner_name, winner_hand, winner_age, minutes, winner_ht, tourney_date), skip = 1)

%>%
        # Ensure you only load the csv's, not the README.txt.
        .[grepl("atp_matches-.csv", .)] %>%
        as.list() %>%
        map_df(~silentread(.)) 
        # equivalent to using map_df
datcolat
}

TennisData <-Data_Collating("Question3/data/Tennis")
```


# Question 4: Netflix streaming: Understanding what works and what doesn't

We want to join the two datasets via 'id' variable. We are interested in seeing what 
```{r}
library(tidyverse)

NetflixDataActors <- read.csv("Question4/data/netflix/credits.csv")
NetflixDataTitles <- read.csv("Question4/data/netflix/titles.csv")
```


What works for Netflix and what does not? We left join the two data frames according to matching the 'id' variable. We then create a data frame that has only the imdb_score, type, release_year, runtime, genres and production_countries variables.

# Figure 1: Year of release and IMDB scores

For movies, we can see that older releases get higher imdb scores. Newer movies do not get the best ratings, so should consider hosting older classic movies.
```{r}
source('Question4/code/NetflixDataYearPlot.R')

NetflixDataYearPlot(data1=NetflixDataActors, data2=NetflixDataTitles)

```

# Figure 2: Run time and imdb scores

Likely that the reduction after 60 minutes is captured by the SERIES variables in the data set and that there are increasing marginal returns to a longer movie, up until a point of around just before 200 minutes.
```{r}
source('Question4/code/NetflixRuntimePlot.R')

NetflixDataRuntimePlot(data1=NetflixDataActors, data2=NetflixDataTitles)


```
# Table showing which actors yield the best IMDB scores

Which best actors to include in movies? The only actors in movies with an IMDB score higher than 9.4 are as follows in the table.
```{r}
NetflixData %>% group_by(name) %>% filter(role=='ACTOR' & imdb_score>9.4) %>% select(name, imdb_score)

tabl <- "
| Name of actor | IMDB Score |
|---------------|:----------:|
|Bryan Cranston | 9.5        | 
|Aaron Paul     | 9.5        | 
|Anna Gunn      | 9.5        | 
|Dean Norris    |9.5         |
|Jonathan Banks |9.5         |
|Bob Odenkirk   |9.5         |
|Betsy Brandt   |9.5         |
|RJ Mitte       |9.5         |
"
cat(tabl)
```

