str_extract_digs <- function(s) {
  library(stringr)
  exp <- '([0-9]+)'
  charlist <- str_extract_all(s, exp)
  digs <- as.numeric(unlist(charlist))
  digs
} 