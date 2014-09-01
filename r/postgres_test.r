source('r/functions.r')

library(dplyr)

config <- read_config()

ep_db <- src_postgres(dbname = config$db$db, 
                      host = config$db$host, 
                      user = config$db$user,
                      password = config$db$pass)