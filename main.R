library(magrittr)

list_id <- 734801183
api_key <- "hwzb5i6w12s50molpefovr4c"
list_data <-
  jsonlite::fromJSON(paste0(
    "https://openapi.etsy.com/v2/listings/",
    list_id,
    "?api_key=",
    api_key
  ))

