library(magrittr)
library(dplyr)

api_key <- read.csv("api_key.txt", header = F) %>% unlist()
listing_id <- 734801183

# test 
library("lubridate")
as_datetime(1602639304)   
as.POSIXct(1602639304, origin = "1970-01-01")
(Sys.time() - as_datetime(1602639304)) %>% as.numeric()