library(dplyr)

# ep_pass <- 

ep_db <- src_postgres(dbname = "ep", 
                      host = "localhost", 
                      user = "ep",
                      password = ep_pass)