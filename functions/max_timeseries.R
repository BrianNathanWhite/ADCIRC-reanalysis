# compute block maxima for given station using only those years which the proportion of NAs are < threshold
max_timeseries <- function(timeseries, threshold) {
  
  timeseries_na <- overview_na(timeseries, threshold, T)
  results <- list()
  
  for(station in colnames(timeseries[, 5:ncol(timeseries)])) {
    
    df <- timeseries %>%
      select(year, station) %>%
      filter(!year %in% timeseries_na$station) %>%
      drop_na() %>%
      group_by(year) %>%
      summarise(max = max(eval(as.name(station))))
    
    results[[station]] <- df
  }
  return(results)
}
