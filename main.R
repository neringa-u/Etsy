library(magrittr)

list_id <- 734801183
api_key <- read.delim(file.choose(), header = F) %>% 
  unlist %>% as.character
list_data <-
  jsonlite::fromJSON(paste0(
    "https://openapi.etsy.com/v2/listings/",
    list_id,
    "?api_key=",
    api_key
  ))

