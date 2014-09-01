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
                      text.name = 'text', 
                      time.name = 'time') {
  if(length(id) > 1) return(dplyr::rbind_all(lapply(id, melt_text, data = data,
                                                    deadline = deadline,
                                                    id.name = id.name,
                                                    text.name = text.name)))
  
  text <- data[[text.name]][data[[id.name]]==id]
  numbers <- str_extract_numbs(text)
  hour <- reported_hour(data[[time.name]][data[[id.name]]==id], deadline)
  data.frame(id = id,
             hour = hour,
             position = seq_along(numbers),
             value = numbers)
}

read_config <- function(filename = 'config.yml') {
  library(yaml)
  yaml.load_file('config.yml')
}