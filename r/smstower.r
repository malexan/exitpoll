smstower_getdata <- function(user, passwd, used) {
  
  library(RCurl)
  library(dplyr)
  library(lubridate)
  
  loginurl <- "http://clients.smstower.ru/login.php"
  dataurl  <- "http://clients.smstower.ru/received.php?stat_csv"
  
  pars <- list(
    Submit = "1",
    login = user,
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

smstower_sendsms <- function(text, recipient, sender, user, passwd) {
  
  library(RCurl)
  url <- "http://clients.smstower.ru/sender.v2.php"
  pars <- list(
    login = user,
    password = passwd,
    phone = recipient,
    sms = text,
    sender = sender
  )
  
  # Suppress warning "Found possible curl options in form parameters: password"
  response <- suppressWarnings(postForm(url, .params = pars))
  response
  
}