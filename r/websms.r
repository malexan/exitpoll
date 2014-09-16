websms_getdata <- function(user, passwd, startdate, enddate) {
  library(httr)
  library(XML)
  cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
  
  url <- 'https://websms.ru/'
  path <- 'http_feedback.asp'
  r <- GET(url, path = path, query = list(http_username = user,
                                          http_password = passwd,
                                          startdate = startdate,
                                          enddate = enddate,
                                          format = "xml"),
           config(cainfo = cafile, ssl.verifypeer = FALSE)) #,
                 # encoding = "CP1251"))
  
  r1 <- try(xmlParse(r))
  if(!exists('r1')) {
    library(lubridate)
    time <- format(dmy_hms(startdate) + minutes(1), format("%d.%m.%Y %H:%M:%S"))
    return(data.frame(time = time, 
               text = "1111",
               agent = "79532319631",
               stringsAsFactors = F))
  }
  r <- xmlToList(r1)
  if (r[[1]][[2]][[1]] == "no records found") return (data.frame())
  r <- dplyr::rbind_all(lapply(r, function(x) {
    if(names(x)[1] == 'sms_date') { # In case of sms with empty body
      return(data.frame(time = x[['sms_date']],
                        text = "",
                        agent = x[['telnum']],
                        stringsAsFactors = F))}
    
    data.frame(time = x$.attrs[['sms_date']], 
               text = x$text,
               agent = x$.attrs[['telnum']],
               stringsAsFactors = F)
  }))
  r$time <- lubridate::dmy_hms(r$time)
  r  
}


websms_sendsms <- function(text, recipient, sender, user, passwd) {
  library(httr)
  library(stringr)
  
  if(length(recipient) > 1) recipient <- str_c(recipient, collapse = ',')
  
  cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
  
  url <- 'https://websms.ru/'
  path <- 'http_in5.asp'
  
  r <- GET(url, path = path, query = list(http_username = user,
                                          http_password = passwd,
                                          fromPhone = sender,
                                          Phone_list = recipient,
                                          Message = text),
           config(cainfo = cafile, ssl.verifypeer = FALSE,
                  encoding = "CP1251"))
  r
}