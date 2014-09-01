str_extract_digs <- function(s) {
  if(length(s) > 1) return(lapply(s, str_extract_digs))
  library(stringr)
  exp <- '([0-9]+)'
  charlist <- str_extract_all(s, exp)
  as.integer(unlist(charlist))
}