setwd('/home/ep/exitpoll/')

rm(list = ls())

source('r/functions.r')
source('r/read_config.r')
source('r/connect_db.r')
source('r/websms.r')
source('r/proc_data.r')

proc_data()

