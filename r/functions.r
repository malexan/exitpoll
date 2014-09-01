str_extract_numbs <- function(s) {
  if(length(s) > 1) return(lapply(s, str_extract_digs))
  library(stringr)
  exp <- '([0-9]+)'
  charlist <- str_extract_all(s, exp)
  as.integer(unlist(charlist))
}

melt_text <- function(id, data, id.name = 'id', text.name = 'text') {
  if(length(id) > 1) return(dplyr::rbind_all(lapply(id, melt_text, data = data,
                                   id.name = id.name,
                                   text.name = text.name)))
  
  text <- data[[text.name]][data[[id.name]]==id]
  numbers <- str_extract_numbs(text)
  data.frame(id = id,
             position = seq_along(numbers),
             value = numbers)
}