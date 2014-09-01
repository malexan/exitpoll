get_smstower <- function(login, passwd, used) {
  
  library(RCurl)
  library(dplyr)
  library(lubridate)
  
  loginurl <- "http://clients.smstower.ru/login.php"
  dataurl  <- "http://clients.smstower.ru/received.php?stat_csv"
  
  pars <- list(
    Submit = "1",
    login = login,
    pass = passwd
  )
  
  agent="Mozilla/5.0"
  
  curl <- getCurlHandle()
  curlSetOpt(cookiejar="", useragent = agent, followlocation = TRUE, curl=curl)
  
  login <- postForm(loginurl, .params = pars, curl = curl)
  rm(login)
  
  data <- getURL(dataurl, curl = curl)
  
  data <- read.table(text = data, header = F, sep = ';', quote="\"",
                     col.names = c('id', 'text', 'time', 'rec', 'agent', 'empty'),
                     colClasses = c('character', 'character', 'character', 
                                    'character', 'character', 'character'),
                     stringsAsFactors = F)
  
  data <- data %>%
    select(id, text, time, agent) %>%
    mutate(id = as.integer(id)) %>%
    filter(!(id %in% used)) %>% 
    mutate(time = dmy_hm(time))
  
  data
}