connect_db <- function(config) {
  
  db <- src_postgres(dbname = config$db$db, 
                     host = config$db$host, 
                     user = config$db$user,
                     password = config$db$pass)
  db
}
