# computes the number of NA values
sum_na <- function(x) sum(is.na(x))

# computes proportion of NA values
prop_na <- function(x) sum_na(x)/length(x)

# computes row index of NA values
which_na <- function(x) which(is.na(x) == T)

# if the proportion of NA values in a station (whether overall or within a year) exceeds this then it will be flagged for removal
overview_na <- function(timeseries, threshold, group_by_year = F) {
  
  if(group_by_year == F) {
    
    # computes the proportion of NA values as each station and then returns T or F if value exceeds threshold
    df1 <- timeseries %>%
      dplyr::select(-TIME, -year, -month, -day) %>%
      summarise_all(prop_na) >  threshold
    
    # returns vector of station names to be excluded
    stations_to_exclude <- df_meta_region3$stationname[df1]
    return(stations_to_exclude)
    
  } else {
    
    # computes the proportion of NA values for each year for each station
    df1 <- timeseries %>%
      dplyr::select(-TIME, -month, -day) %>%
      group_by(year) %>%
      summarise_all(prop_na) %>%
      dplyr::select(-year) > threshold
    
    # list which contains the years which should be removed for each station.
    years_to_exclude <- list()
    
    # fill the list
    for(station in 1:ncol(df1)) {
      
      df2 <- df1[, station]
      
      years_to_exclude_by_station <- unique(timeseries$year)[df2]
      years_to_exclude[[station]] <- years_to_exclude_by_station
    }
    
    names(years_to_exclude) <- df_meta_region3$stationname
    return(years_to_exclude)
    
  }
  
}