retrieve <- function(){
  
  #Retrieve and tag
  data <- dep_hive_query("SELECT dt,ip,x_forwarded_for,referer,user_agent, uri_path AS page FROM
                        wmf_raw.webrequest WHERE year = 2014 AND month = 12 AND day
                        BETWEEN 15 AND 21 AND uri_host IN('en.wikipedia.org','en.m.wikipedia.org')
                        AND uri_path IN ('/wiki/FSArchiver','/wiki/Additive_white_Gaussian_noise',
                        '/wiki/Ariana_Grande','/wiki/Fidel_Castro','/wiki/Surfers_Paradise_Meter_Maids',
                        '/wiki/Fugio_Cent') AND webrequest_source IN('text','mobile');")
  data <- data[data$uri_path %in% c(bots,humans,plausibly_bots),]
  data$type[data$uri_path %in% bots] <- "bot"
  data$type[data$uri_path %in% humans] <- "human"
  data$type[data$uri_path %in% plausibly_bots] <- "plausibly bots"
  
  #Geolocate to identify country and TZ
  data$ip[!data$x_forwarded_for == "-"] <- data$x_forwarded_for[!data$x_forwarded_for == "-"]
  data <- cbind(data,geolookup(data$ip, filename = "/usr/local/share/GeoIP/GeoIP2-City.mmdb",
                               c("country_iso","timezone")))
  
  #Localise timestamps
  data <- data[,j={
    subset_copy <- copy(.SD)
    localised_stamps <- with_tz(log_strptime(dt), timezone)
    subset_copy$hour <- hour(localised_stamps)
    subset_copy
  }, by = "timezone"]
  
  #Handle XFFs and generate hashes
  data$fingerprint <- cryptohash(paste0(data$ip,data$user_agent),"md5")
  
  #Sanitise
  data$page <- gsub(x = data$page, pattern = "/wiki/", replacement = "", fixed = TRUE)
  data$page <- gsub(x = data$page, pattern = "_", replacement = " ", fixed = TRUE)
  
  #Return
  return(data)
  
}