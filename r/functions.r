library(dplyr)
library(lubridate)
library(stringr)



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

manual_add <- function(text) {
  numbers <- str_extract_numbs(text)
  hour <- numbers[1]
  numbers <- numbers[-1]
  df <- data.frame(sms = 111111,
                   position = seq_along(numbers),
                   value = numbers,
                   hour = hour)
  df$station <- df$value[df$position == 1]
  df <- df[df$position != 1,]
  DBI::dbWriteTable(ep_db$con, 'data', df, append = T, row.names = F)
}


simulate_report <- function(station, hour, candidates, 
                            turnout.prob, refuse.prob,
                            stationsdata) {

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
  
  numbs <- str_extract_numbs(sms)
  if(length(numbs) == 0) return(1)
   
  if(!(numbs[1] %in% stations$station)) return(2)

  station <- numbs[1]
  type <- stations$reserve[stations$station == station]
  district <- stations$district[stations$station == station]
  size <- 8
  if(type == 2 & district == 11) size <- size + 6
  if(type == 2 & district == 20) size <- size + 6
  if(type == 3 & district == 11) size <- 8
  if(type == 3 & district == 20) size <- 8
  if(length(numbs) != size) return(3)
  
  
  
  if(type %in% c(0,3) & sum(numbs[3:8]) > numbs[2]) return(4)
  if(type == 2 & (sum(numbs[3:8]) > numbs[2] |
                    sum(numbs[9:14]) > numbs[2])) return(4)
    
  return(0)
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
  data %>% select(station, name, voters, address, reserve, district)
}
  
