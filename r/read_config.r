read_config <- function(filename = 'config.yml') {
  library(yaml)
  return(yaml.load_file('config.yml'))
}
