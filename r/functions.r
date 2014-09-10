library(dplyr)
library(lubridate)
library(stringr)

source('r/smstower.r')
source('r/websms.r')
source('r/proc_data.r')

str_extract_numbs <- function(s) {
  if(length(s) > 1) return(lapply(s, str_extract_numbs))
  library(stringr)
  exp <- '([0-9]+)'
  charlist <- str_extract_all(s, exp)
  as.integer(unlist(charlist))
}

reported_hour <- function(smstime, deadline) {
  if(length(smstime) > 1) return(unlist(lapply(smstime, 
                                               reported_hour,
                                               deadline = deadline)))
  library(lubridate)
  ifelse(minute(smstime) < deadline, hour(smstime), hour(smstime) + 1)
}

melt_text <- function(data, deadline) {

  rbind_all(lapply(seq_len(nrow(data)), 
                   function(r) {
                     text <- data$text[r]
                     numbers <- str_extract_numbs(text)
                     hour <- reported_hour(data$time[r], deadline)
                     df <- data.frame(sms = data$id[r],
                                      position = seq_along(numbers),
                                      value = numbers,
                                      hour = hour)
                     df$station <- df$value[df$position == 1]
                     df <- df[df$position != 1,]
                     df
                     }))
}

read_config <- function(filename = 'config.yml') {
  library(yaml)
  return(yaml.load_file('config.yml'))
}

# initiate_db <- function



simulate_report <- function(station, hour, candidates, 
                            turnout.prob, refuse.prob,
                            stationsdata) {
  
  library(stringr)
  
  if(missing(turnout.prob)) turnout.prob <- 
    c(123, 204, 327, 312, 372, 301, 253, 175, 149, 116, 116, 116)/12791
  
  if(missing(refuse.prob)) refuse.prob <- 664/2332
  
  if(length(station) > 1) return(dplyr::rbind_all(
    lapply(station, simulate_report, hour = hour, candidates = candidates, 
           turnout.prob = turnout.prob, refuse.prob = refuse.prob,
           stationsdata = stationsdata)))
  
  if(!(station %in% stationsdata$station)) 
    stop (paste("Station #", station, " is not in the database", sep = ''))
  
  turnout <- round(stationsdata$voters[stationsdata$station == station] *
                     rnorm(1, mean = turnout.prob[hour - 8], sd = 0.01))
  if(turnout < 0) turnout <- 0
  
  refuse <- round(turnout * rnorm(1, refuse.prob, .02))
  if(refuse < 0) refuse <- 0
  
  if(length(candidates) == 1) candid.probs <- rep(1/candidates, candidates)
  if(length(candidates) > 1) candid.probs <- candidates
  candidates <- rmultinom(1, size = turnout - refuse, prob = candid.probs)[,1]
  
  text <- str_c(station, turnout, refuse, 
                  str_c(candidates, collapse = ' '), sep = ' ')
  
  if(!exists(".simulated_sms_id")) .simulated_sms_id <<- 10000 else
    .simulated_sms_id <<- .simulated_sms_id + 1
  
  report <- data.frame(id = .simulated_sms_id, 
                       text = text,
                       time = now(),
                       agent = config$testphone,
                       stringsAsFactors = F)
  
  report
}

add_used_sms <- function(new_used, conn, table) {
  library(DBI)
  dbWriteTable(conn$con, table,
               data.frame(id = new_used), 
               append = T, row.names = F)
  
}

validate_sms <- function(sms, stations, test.types) {
  if(length(sms) > 1) stop ("Only one sms allowed")
  if(missing(test.types)) test.types <- c('digits', 'uik',
                                          'amount', 'sum')
  
  numbs <- str_extract_numbs(sms)
  
  if('digits' %in% test.types) {
      if(length(numbs) == 0) return(1)
    }
    
  if('uik' %in% test.types) {
    if(!(numbs[1] %in% stations$station)) return(2)
  }
  
  if('amount' %in% test.types) {
    # TODO: Amount of numbers hardcoded
    if(length(numbs) != 8) return(3)
  }
  
  if('sum' %in% test.types) {
    if(sum(numbs[3:8]) > numbs[2]) return(4)
  }
    
  return(0)
}


demo_run <- function() {
  library(dplyr)
  library(lubridate)
  library(stringr)
  
  config <- read_config()
  
  ep_db <- src_postgres(dbname = config$db$db, 
                        host = config$db$host, 
                        user = config$db$user,
                        password = config$db$pass)
  
  checks <- collect(tbl(ep_db, 'checks1'))
  last_check <- checks$time[dim(checks)[1]]
  last_check <- format(dmy_hms(last_check) + seconds(1),
                       format("%d.%m.%Y %H:%M:%S"))
  
  data <- websms_getdata(config$websms$user, 
                         config$websms$pass,  
                         startdate = last_check, 
                         enddate = "14.09.2014")
  
  if(nrow(data) == 0) {
    DBI::dbDisconnect(ep_db$con)
    return()
  }
  
  
  data <- cbind(data, status = apply(
    data, 1, function(x) validate_sms(x[['text']])))
  
  apply(data, 1, function(x) {
    if(x[['status']] == 0) {
      websms_sendsms("Vashe sms prinyato. Oshibok ne obnaruzheno",
                     recipient = x[['agent']],
                     config$websms$sender,
                     config$websms$user, 
                     config$websms$pass)
      return()
    }
    request_correction(x)
  })
  
  last_check <- data$time[dim(data)[1]]
  last_check <- format(last_check,
                       format("%d.%m.%Y %H:%M:%S"))
  lastcheck_sql <- str_c(
"INSERT INTO checks1(time) 
VALUES ('", last_check, "') ;")
  
  DBI::dbSendQuery(ep_db$con, lastcheck_sql)

  DBI::dbDisconnect(ep_db$con)
}
  
  
request_correction <- function(sms, config) {
  if(as.integer(sms[['status']]) == 0) {
    warning('SMS is correct, request is not required')
    return()}
  request_text <- function(error_code) {
    switch(as.integer(error_code),
           "Oshibka: net chisel v sms-otchete",
           "Oshibka: neizvestniy nomer uchastka",
           "Oshibka: nepravilnoe kolichestvo chisel v sms-otchete",
           "Oshibka: summa golosov i otkazov bolshe yavki")}
  request <- request_text(sms[['status']])
  websms_sendsms(request, 
                 recipient = sms[['agent']],
                 config$websms$sender,
                 config$websms$user, 
                 config$websms$pass)
}

add_sms_id <- function(data, ep_db) {
  sms_sql <- tbl(ep_db, 'sms')
  last_old_sms_id <- sms_sql %>% 
    select(id) %>%
    filter(id == max(id)) %>%
    collect %>% unlist %>% unname
  data$id <- (last_old_sms_id + 1):(last_old_sms_id + nrow(data))
  data
}

connect_db <- function(config) {
  
  db <- src_postgres(dbname = config$db$db, 
                        host = config$db$host, 
                        user = config$db$user,
                        password = config$db$pass)
  db
}

get_last_check <- function(ep_db) {
  checks <- collect(tbl(ep_db, 'checks1'))
  last_check <- checks$time[dim(checks)[1]]
  last_check <- format(dmy_hms(last_check) + seconds(1),
                       format("%d.%m.%Y %H:%M:%S"))
  last_check
}

get_stations <- function(ep_db) {
  stations <- collect(tbl(ep_db, 'stations'))
  districts <- collect(tbl(ep_db, 'districts'))
  data <- inner_join(stations, districts, by = 'district')
  data %>% select(station, name, voters, address, reserve)
}
  
