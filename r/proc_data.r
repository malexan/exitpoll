
proc_data <- function(deadline = 40, simulate) {
  library(dplyr)
  
  config <- read_config()
  ep_db <- connect_db(config)
  last_check <- get_last_check(ep_db)
  stations <- get_stations(ep_db)
  
  

  
  data <- websms_getdata(config$websms$user, 
                         config$websms$pass,  
                         startdate = last_check, 
                         enddate = "14.09.2014")  
  
  if(nrow(data) == 0) {
    DBI::dbDisconnect(ep_db$con)
    return()
  }
  
  data <- cbind(data, 
                status = apply(
    data, 1, function(x) validate_sms(x[['text']], stations)))
  
  data <- add_sms_id(data, ep_db)
  
  
  ### Send confirmation or request answer
  apply(data, 1, function(x) {
    if(x[['status']] == 0) {
      websms_sendsms("Vash sms-otchet prinyat. Oshibok ne obnaruzheno. Spasibo!",
                     recipient = x[['agent']],
                     config$websms$sender,
                     config$websms$user, 
                     config$websms$pass)
      return()
    }
    request_correction(x, config)
  })
  
  ### Update in DB time of last processed sms
  last_check <- data$time[dim(data)[1]]
  last_check <- format(last_check,
                       format("%d.%m.%Y %H:%M:%S"))
  lastcheck_sql <- str_c(
    "INSERT INTO checks1(time) 
    VALUES ('", last_check, "') ;")
  DBI::dbSendQuery(ep_db$con, lastcheck_sql)
  
  ### Save raw sms

  DBI::dbWriteTable(ep_db$con, 'sms', data, append = T, row.names = F)
  
  ### Save valid data in DB
  
  valid_data <- data %>%
    filter(status == 0) %>%
    melt_text(deadline)
  DBI::dbWriteTable(ep_db$con, 'data', valid_data, append = T, row.names = F)
  
  # Save stations' status
  stations_status <- data %>%
    mutate(station = as.integer(str_extract(text, '([0-9]+)'))) %>%
    filter(!is.na(station)) %>%
    filter(station %in% stations$station) %>%
    mutate(time = format(time, format("%Y-%m-%d %H:%M:%S"))) %>%
    select(id, station, time, status)
  DBI::dbWriteTable(ep_db$con, 'status', 
                    stations_status, append = T, row.names = F)           
  
  DBI::dbDisconnect(ep_db$con)
}
