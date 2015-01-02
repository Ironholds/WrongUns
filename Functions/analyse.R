analyse <- function(x){
  
  #Calculate fingerprints
  fingerprints <- x[, j = {
    lcolc <- Lc(table(fingerprint))
    lcdf <- data.frame(L = rev(1-lcolc$L), p = lcolc$p, Uprob = c(1:length(lcolc$L)/length(lcolc$L)),
                       type = type[1], stringsAsFactors = FALSE)
  }, by = c("page")]
  
  ggsave(plot = 
    ggplot(fingerprints, aes(x = Uprob, y = L, type = page, group = page)) + 
      geom_line(aes(linetype = page, colour = type)) + geom_line(aes(x = p, y = p)) +
      labs(title = "Distribution of requests by (ip, user agent) fingerprints",
           x = "Proportion of fingerprints",
           y = "Proportion of requests"),
    file = file.path(getwd(),"Graphs", "fingerprint_lorenz.png"))
  
  referers <- x[, j = {
    lcolc <- Lc(table(referer))
    lcdf <- data.frame(L = rev(1-lcolc$L), p = lcolc$p, Uprob = c(1:length(lcolc$L)/length(lcolc$L)),
                       type = type[1], stringsAsFactors = FALSE)
  }, by = c("page")]
  ggsave(plot = 
    ggplot(referers, aes(x = Uprob, y = L, type = page, group = page)) + 
      geom_line(aes(linetype = page, colour = type)) + geom_line(aes(x = p, y = p)) +
      labs(title = "Distribution of requests by referer",
           x = "Proportion of referers",
           y = "Proportion of requests"),
  file = file.path(getwd(),"Graphs", "referer_lorenz.png"))
  
  agents <- x[, j = {
    lcolc <- Lc(table(user_agent))
    lcdf <- data.frame(L = rev(1-lcolc$L), p = lcolc$p, Uprob = c(1:length(lcolc$L)/length(lcolc$L)),
                       type = type[1], stringsAsFactors = FALSE)
  }, by = c("page")]
  ggsave(plot = 
           ggplot(agents, aes(x = Uprob, y = L, type = page, group = page)) + 
           geom_line(aes(linetype = page, colour = type)) + geom_line(aes(x = p, y = p)) +
           labs(title = "Distribution of requests by user agent",
                x = "Proportion of user agents",
                y = "Proportion of requests"),
         file = file.path(getwd(),"Graphs", "agent_lorenz.png"))
  
  temporal <- x[,j={(.SD[sample(1:.N,100000),])},by = c("page","type")]
  temporal <- temporal[,j=list(requests=.N),by = c("hour","page","type")]
  ggsave(plot = 
    ggplot(temporal, aes(x = hour, y = requests, type = page, group = page)) + 
      geom_line(aes(linetype = page, colour = type), size = 1.2) +
      labs(title = "Distribution of requests by hour (localised)",
           x = "hour", y = "requests"),
    file = file.path(getwd(),"Graphs", "requests_by_hour.png"))
}