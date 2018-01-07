+++
css = []
title = "Hubway Visualization Challenge"
date = 2018-01-07T20:09:03+05:30
draft = false
description = ""
tags = []
scripts = []
highlight = true
+++
Visualizing Hubway Data
=======================

Introduction
------------

While browsing around for an exploratory data analysis project, I came across the [Hubway Data Visualization Challenge](http://hubwaydatachallenge.org/). Hubway is a bicycle rental company that has stations throughout in Boston and the surrounding areas. The competition has already closed but the data was still available through the site. Having lived in Boston for a few years, I decided to play around with the data.

Objective
---------

In this blog, I will try to document the steps that I took to create my rendering of geospatial visualizations of Hubway's bicycle data. I would share with you how I approached this project.

Heading into the real work!
---------------------------

### Helper Functions

Most of the code has been taken from [Howard's github.](https://github.com/feelosophy13)

#### Function to load data

Functions used to load data from the data directory.

``` r
## load data
loadData <- function() {
  missingTypes <- c(NA, '', ' ')
  trips <<- read.csv('./data/hubway_trips.csv', na.strings=missingTypes, stringsAsFactors=FALSE)
  stations <<- read.csv('./data/hubway_stations.csv', na.strings=missingTypes, stringsAsFactors=FALSE)  
}
```

#### Function to load libraries

Functions to load the required packages.

``` r
# load libraries
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
```

#### Loading the libraries

Now, we will load the important libraries ***dplyr*** and \*\*\_reshape2\*\*

``` r
loadLibraries <- function() {
  usePackage('dplyr')
  usePackage('reshape2')
}
```

#### Formating the data

Now, we will format the data sources and fix the data types such that data becomes clean and an effective analysis can be performed on it.

``` r
# casting data types

casteDataTypeForTripsDF <- function(tripsDF) {
  tripsDF$seq_id <- NULL
  tripsDF$status <- NULL
  tripsDF$start_date <- strptime(tripsDF$start_date, format='%m/%d/%Y %H:%M:%S')
  tripsDF$end_date <- strptime(tripsDF$end_date, format='%m/%d/%Y %H:%M:%S')
  tripsDF$bike_nr <- as.factor(tripsDF$bike_nr)
  tripsDF$subsc_type <- as.factor(tripsDF$subsc_type)
  tripsDF$zip_code <- NULL
  tripsDF$birth_date <- as.factor(tripsDF$birth_date)
  tripsDF$gender <- as.factor(tripsDF$gender)
  return(tripsDF)
}

casteDataTypeForStations <- function(stationsDF) {
  stationsDF$terminal <- as.factor(stationsDF$terminal)
  stationsDF$station <- as.factor(stationsDF$station)
  stationsDF$municipal <- as.factor(stationsDF$municipal)
  stationsDF$status <- as.factor(stationsDF$status)
  return(stationsDF)
}
```

#### Removing the missing rows

Basic data cleaning to remove inconsistent data.

``` r
removeRowsMissStns <- function(tripsDF) {
  tripsDF <- subset(tripsDF, !is.na(strt_statn) & !is.na(end_statn))
  return(tripsDF)
}
```

#### Aggregating the data

Now we will further aggregate the given datasets to produce our own subsets of data such that we can perform effective visualizations with them in Tableau without using the Data Interpreter and Dimensions &lt;-&gt; Measures conversions.

``` r
# aggregate trips from station to station
aggTripsS2S <- function(tripsDF) {
  tripsDF$start_date <- as.character(tripsDF$start_date)
  tripsDF$end_date <- as.character(tripsDF$end_date)
  
  grp <- group_by(tripsDF, strt_statn, end_statn)  # set up the grouping
  agg <- dplyr::summarize(grp, cnt=n())  #set up aggregation by groups
  agg <- arrange(agg, cnt)  # order the data
  agg <- collect(agg)  # grab the result
  agg <- as.data.frame(agg)

  return(agg)
}

# add longitude and latitude info for starting stations
addStartLocs <- function(df) {
  df$strtLng <- stations$lng[match(df$strt_statn, stations$id)]
  df$strtLat <- stations$lat[match(df$strt_statn, stations$id)]
  return(df)  
}

# add longitude and latitude info for ending stations
addEndLocs <- function(df) {
  df$endLng <- stations$lng[match(df$end_statn, stations$id)]
  df$endLat <- stations$lat[match(df$end_statn, stations$id)]
  return(df)
}

# add longitude and latitude information to start and end stations
addStartAndEndLocs <- function(trAggDF) {
  trAggDF <- addStartLocs(trAggDF)
  trAggDF <- addEndLocs(trAggDF)
  return(trAggDF)
}
```

Adding some necessary extra columns and performing other aggregating stuff!.

``` r
# dplyr count method
dplyrCnt <- function(grp) {
  agg <- dplyr::summarize(grp, cnt=sum(cnt))  # aggregate
  agg <- arrange(agg, cnt)  # order
  agg <- collect(agg)
  agg <- as.data.frame(agg)
  return(agg)
}

# standardize column names for trip-counts-by-station dfs
stdTripCtnByStnDFColnames <- function(df) {
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub('^end(_)?|^strt(_)?|^start(_)?', '', colnames(df))
  return(df)
}

# add station locations
addStnLocs <- function(df) {
  df$lng <- stations$lng[match(df$statn, stations$id)]
  df$lat <- stations$lat[match(df$statn, stations$id)]
  return(df)
}

# aggregate number of incoming/outgoing trips by station id
aggTripCntByStn <- function(trAggDF) {

  # incoming trip counts by station
  grp <- group_by(trAggDF, end_statn)
  incTripCntByStnDF <- dplyrCnt(grp)
  incTripCntByStnDF <- stdTripCtnByStnDFColnames(incTripCntByStnDF)
  colnames(incTripCntByStnDF)[2] <- 'inc_cnt'

  # outgoing trip counts by station
  grp <- group_by(trAggDF, strt_statn)
  outTripCntByStnDF <- dplyrCnt(grp)
  outTripCntByStnDF <- stdTripCtnByStnDFColnames(outTripCntByStnDF)  
  colnames(outTripCntByStnDF)[2] <- 'out_cnt'
  
  # combine the two dfs, order, and return
  outputDF <- merge(incTripCntByStnDF, outTripCntByStnDF, by='statn')
  outputDF$tot_cnt <- outputDF$inc_cnt + outputDF$out_cnt
  outputDF <- outputDF[order(outputDF$statn), ]
  return(outputDF)
}

# add incoming and outgoing trip percentages
addIncOutTripPercs <- function(byStnMetricDF) {
  byStnMetricDF$inc_perc <- round(byStnMetricDF$inc_cnt / byStnMetricDF$tot_cnt * 100, 2)
  byStnMetricDF$out_perc <- round(byStnMetricDF$out_cnt / byStnMetricDF$tot_cnt * 100, 2)
  return(byStnMetricDF)
}

# function that takes in station-to-station pairs and collapses directionality of trips
# e.g. stnID4-stnID120 trip and stnID120-stnID4 trip should be combined into one pair (either as stnID4-stnID120 or stnID120-stnID4)
collapseDirectionS2S <- function(trAggDF) {
  output <- trAggDF

  # collapse directionality dimension
  output$strtLng <- output$strtLat <- output$endLng <- output$endLat <- NULL
  output[1:2] <- t(apply(output, 1, function(x) sort(x[1:2])))
  output <- aggregate(cnt ~ ., data=output, FUN=sum)  
  
  # order the output df by strt_statn and end_statn
  output <- output[order(output$strt_statn, output$end_statn), ]

  # add start and end locations
  output <- addStartAndEndLocs(output)
  return(output)
}
```

##### Finally we are done with all the helper functions.

##### Time to run the program!!

#### Program Execution

``` r
# Loading the data and libraries
loadData()
loadLibraries()
```


Performing the data preprocessing tasks

``` r
# preprocessing
stations <- casteDataTypeForStations(stations)
trips <- casteDataTypeForTripsDF(trips)
trips <- removeRowsMissStns(trips)
```

Aggregaring the data.

``` r
# aggregating
trAggS2S <- aggTripsS2S(trips)
trAggS2S <- addStartAndEndLocs(trAggS2S)
ndTrAggS2S <- collapseDirectionS2S(trAggS2S)
byStnMetric <- aggTripCntByStn(trAggS2S)
byStnMetric <- addIncOutTripPercs(byStnMetric)
byStnMetric <- addStnLocs(byStnMetric)
```

Now we are done with loading the datasets and cleaning them. \#\#\#\#\#Let's see them!.

###### Stations data

``` r
head(stations,5)
```

    ##   id terminal                            station municipal      lat
    ## 1  3   B32006             Colleges of the Fenway    Boston 42.34002
    ## 2  4   C32000        Tremont St. at Berkeley St.    Boston 42.34539
    ## 3  5   B32012 Northeastern U / North Parking Lot    Boston 42.34181
    ## 4  6   D32000           Cambridge St. at Joy St.    Boston 42.36129
    ## 5  7   A32000                           Fan Pier    Boston 42.35341
    ##         lng   status
    ## 1 -71.10081 Existing
    ## 2 -71.06962 Existing
    ## 3 -71.09018 Existing
    ## 4 -71.06514 Existing
    ## 5 -71.04462 Existing

###### Trips data

``` r
head(trips,5)
```

    ##   hubway_id duration          start_date strt_statn            end_date
    ## 1         8        9 2011-07-28 10:12:00         23 2011-07-28 10:12:00
    ## 2         9      220 2011-07-28 10:21:00         23 2011-07-28 10:25:00
    ## 3        10       56 2011-07-28 10:33:00         23 2011-07-28 10:34:00
    ## 4        11       64 2011-07-28 10:35:00         23 2011-07-28 10:36:00
    ## 5        12       12 2011-07-28 10:37:00         23 2011-07-28 10:37:00
    ##   end_statn bike_nr subsc_type birth_date gender
    ## 1        23  B00468 Registered       1976   Male
    ## 2        23  B00554 Registered       1966   Male
    ## 3        23  B00456 Registered       1943   Male
    ## 4        23  B00554 Registered       1981 Female
    ## 5        23  B00554 Registered       1983 Female

###### TripAggregatedStationToStation aggregated data

``` r
head(trAggS2S,5)
```

    ##   strt_statn end_statn cnt   strtLng  strtLat    endLng   endLat
    ## 1          3       106   1 -71.10081 42.34002 -71.04586 42.33569
    ## 2          3       119   1 -71.10081 42.34002 -71.07655 42.35041
    ## 3          3       130   1 -71.10081 42.34002 -71.11402 42.37637
    ## 4          3       133   1 -71.10081 42.34002 -71.12511 42.38029
    ## 5          3       136   1 -71.10081 42.34002 -71.08639 42.36610

###### ByStationMetric aggregated data

``` r
head(byStnMetric, 5)
```

    ##   statn inc_cnt out_cnt tot_cnt inc_perc out_perc       lng      lat
    ## 1     3    9260    9734   18994    48.75    51.25 -71.10081 42.34002
    ## 2     4   17472   18057   35529    49.18    50.82 -71.06962 42.34539
    ## 3     5   11058   10630   21688    50.99    49.01 -71.09018 42.34181
    ## 4     6   22655   23322   45977    49.27    50.73 -71.06514 42.36129
    ## 5     7    8883    9163   18046    49.22    50.78 -71.04462 42.35341

###### Creating a dataframe that can be used for Tableau visualization of traffic.

``` r
tableau_path <- data.frame(statn = numeric(),
                           path_id = character(),
                           cnt = numeric(),
                           lat = numeric(),
                           long = numeric())

for (row in c(1:nrow(trAggS2S))) {
  df_new_start <- data.frame(trAggS2S[row, "strt_statn"],paste(as.character(trAggS2S[row, "strt_statn"]),as.character(trAggS2S[row, "end_statn"]), sep = '_'),trAggS2S[row, "cnt"], trAggS2S[row, "strtLng"], trAggS2S[row,"strtLat"])
  names(df_new_start) <- c("statn", "path_id", "cnt", "lat", "long")
  tableau_path <- rbind(tableau_path, df_new_start)
  df_new_end <- data.frame(trAggS2S[row, "end_statn"],paste(as.character(trAggS2S[row, "strt_statn"]),as.character(trAggS2S[row, "end_statn"]), sep = '_'),trAggS2S[row, "cnt"], trAggS2S[row, "endLng"], trAggS2S[row,"endLat"])
  names(df_new_end) <- c("statn", "path_id", "cnt", "lat", "long")
  tableau_path <- rbind(tableau_path, df_new_end)
}
```

###### Let's see it.

``` r
head(tableau_path)
```

    ##   statn path_id cnt       lat     long
    ## 1     3   3_106   1 -71.10081 42.34002
    ## 2   106   3_106   1 -71.04586 42.33569
    ## 3     3   3_119   1 -71.10081 42.34002
    ## 4   119   3_119   1 -71.07655 42.35041
    ## 5     3   3_130   1 -71.10081 42.34002
    ## 6   130   3_130   1 -71.11402 42.37637

### Visualiziations

By embedding Tableau visualizations in rmarkdown <iframe 
  src = "https://public.tableau.com/views/HubwayVisualizationChallenge/FinalStory?:showVizHome=no&:embed=true&:display_count=yes&publish=yes"
  height="650" width="1100"></iframe>
