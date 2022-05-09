# Then, clean the time-series
clean_timeseries <- function(timeseries, metadata) {
  
  # coerce time variable to POSIX class
  timeseries$TIME <- as.POSIXct(timeseries$TIME, format = "%Y-%m-%d %H:%M:%S")
  
  # create year and month variables from TIME column
  timeseries <- add_column(timeseries,  year = format(timeseries$TIME, '%Y'), month = format(timeseries$TIME,    '%m'), .after = 1)
  
  # rename columns with station name
  for(station in metadata$stationname){
    
    # extract station id
    raw_station_id <- metadata %>%
      filter(stationname == !!station) %>%
      select(stationid) %>%
      pull()
    
    # add an X to the beginning to match current variable names
    station_id <- paste('X', raw_station_id, sep = '')
    
    # rename columns for ease of use
    timeseries <- timeseries %>%
      rename(!!station := station_id)
  }
  
  return(timeseries)
  
}