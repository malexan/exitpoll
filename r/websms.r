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
           config(cainfo = cafile, ssl.verifypeer = FALSE,
                  encoding = "CP1251"))
  
  r <- xmlToList(xmlParse(r))
  if (r[[1]][[2]][[1]] == "no records found") return (data.frame())
  r <- dplyr::rbind_all(lapply(r, function(x) {
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