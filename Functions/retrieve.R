retrieve <- function(query){
  
  #Grab
  data <- dep_hive_query(query)
  
  #Geolocate
  data <- cbind(data,geolookup(data$ip, file = "/usr/local/share/GeoIP/GeoIP2-City.mmdb", c("timezone","country_iso")))
  
  #Localise timestamps
  data <-  data[,j={
    
    #Copy .SD
    subset_copy <- copy(.SD)
    
    #Grab timestamp
    localised_stamps <- with_tz(log_strptime(dt), timezone)
    
    #Extract and include weekday and hour
    subset_copy$hour <- hour(localised_stamps)
    subset_copy$day <- as.character(x = wday(x = localised_stamps, label = TRUE))
    
    #Return
    subset_copy
    
  }, by = "timezone"]
  
  #Extract ua_tuples and graph
  ua_tuple <- paste0(data$ip,data$user_agent)
  holding <- unique(ua_tuple)
  ua_tuple$Var1 <- rownames(ua_tuple)
}