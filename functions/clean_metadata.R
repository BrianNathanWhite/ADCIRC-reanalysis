clean_metadata <- function(metadata) {
  
  # remove white-space from station names as this can lead to conflicts with ggplot
  metadata$stationname <- str_replace_all(metadata$stationname, " ", "")
  
  return(metadata)
  
}