str_extract_numbs <- function(s) {
  if(length(s) > 1) return(lapply(s, str_extract_digs))
  library(stringr)
  exp <- '([0-9]+)'
  charlist <- str_extract_all(s, exp)
  as.integer(unlist(charlist))
}

melt_text <- function(id, text) {
  numbers <- str_extract_numbs(text)
  data.frame(id = id,
             position = seq_along(numbers),
             value = numbers)
}