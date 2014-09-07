source('r/smstower.r')


str_extract_numbs <- function(s) {
  if(length(s) > 1) return(lapply(s, str_extract_digs))
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

melt_text <- function(id, data, deadline,
                      id.name = 'id', 
                      text.name = 'text') {
  if(length(id) > 1) return(dplyr::rbind_all(lapply(id, melt_text, data = data,
                                                    deadline = deadline,
                                                    id.name = id.name,
                                                    text.name = text.name)))
  
  text <- data[[text.name]][data[[id.name]]==id]
  numbers <- str_extract_numbs(text)
  data.frame(id = id,
             position = seq_along(numbers),
             value = numbers)
}

read_config <- function(filename = 'config.yml') {
  library(yaml)
  yaml.load_file('config.yml')
}

# initiate_db <- function



simulate_report <- function(station, hour, candidates, 
                            turnout.prob, refuse.prob,
                            stationsdata) {
  
  library(stringr)
  
  if(missing(turnout.prob)) turnout.prob <- 
    c(123, 204, 327, 312, 372, 301, 253, 175, 149, 116, 116, 116)/12791
  
  if(missing(refuse.prob)) refuse.prob <- 664/2332
  
  if(length(station) > 1) return(unlist(
    lapply(station, simulate_report, hour = hour, candidates = candidates, 
           turnout.prob = turnout.prob, refuse.prob = refuse.prob,
           stationsdata = stationsdata)))
  
  turnout <- round(stationsdata$voters[stationsdata$station == station] *
                     rnorm(1, mean = turnout.prob[hour - 8], sd = 0.01))
  if(turnout < 0) turnout <- 0
  
  refuse <- round(turnout * rnorm(1, refuse.prob, .02))
  if(refuse < 0) refuse <- 0
  
  if(length(candidates) == 1) candid.probs <- rep(1/candidates, candidates)
  if(length(candidates) > 1) candid.probs <- candidates
  
  candidates <- rmultinom(1, size = turnout - refuse, prob = candid.probs)[,1]
  
  report <- str_c(station, turnout, refuse, 
                  str_c(candidates, collapse = ' '), sep = ' ')
  report
}

add_used_sms <- function(new_used, conn, table) {
  library(DBI)
  dbWriteTable(conn$con, table,
               data.frame(id = new_used), 
               append = T, row.names = F)
  
}



full_run <- function(simulate) {
  library(dplyr)
  source('r/functions.r')
  
  config <- read_config()
  
  ep_db <- src_postgres(dbname = config$db$db, 
                        host = config$db$host, 
                        user = config$db$user,
                        password = config$db$pass)
  
  used_remote_tbl <- tbl(ep_db, "used")
  
  data <- smstower_getdata(config$smstower$user, 
                          config$smstower$pass, 
                          as.data.frame(used_remote_tbl)$id[-100])
  
  new_used_sms <-data$id
  
}