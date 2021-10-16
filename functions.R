# get.listing -------------------------------------------------------------

get.listing <- function(listing_id) {
  
  require(jsonlite)
  require(lubridate)
  
  # Address
  x_link <- paste0("https://openapi.etsy.com/v2/listings/",
                 listing_id,
                 "?api_key=",
                 api_key) 
  
  # Get data
  x <- fromJSON(x_link) %>% .[["results"]] 
  
  # Filter data
  x %<>%
    select(listing_id,
           state,
           user_id,
           title,
           description,
           price,
           currency_code,
           original_creation_tsz,
           last_modified_tsz,
           tags,
           materials,
           featured_rank,
           url,
           views,
           num_favorers,
           occasion, 
           style,
           taxonomy_id,
           is_vintage
    ) %>% 
    as_tibble()
  
  # Transform time
  x %<>% mutate_at(vars(contains("_tsz")), ~as_datetime(.))
  
  # Return results
  return(x)
  
}


# get.listings ------------------------------------------------------------

get.listings <- function(search_key, min_price = 0, max_price = 50, limit = 100) {
  
  require(jsonlite)
  require(lubridate)
  
  # Address
  x_link <- paste0("https://openapi.etsy.com/v2/listings/active?api_key=",
                   api_key,
                   "&keywords=", 
                   gsub(" ", "+", search_key),
                   "&min_price=",
                   min_price,
                   "&max_price=",
                   max_price,
                   "&sort_on=score&limit=",
                   limit)
  
  # Get data
  x <- fromJSON(x_link) %>% .[["results"]] 
  
  # Filter data
  x %<>%
    select(listing_id,
           state,
           user_id,
           title,
           description,
           price,
           currency_code,
           original_creation_tsz,
           last_modified_tsz,
           tags,
           materials,
           featured_rank,
           url,
           views,
           num_favorers,
           occasion, 
           style,
           taxonomy_id,
           is_vintage
    ) %>% 
    as_tibble()
  
  # Transform time
  x %<>% mutate_at(vars(contains("_tsz")), ~as_datetime(.))
  
  # Return results
  return(x)
}


# get.user.profile --------------------------------------------------------

get.user.profile <- function(user_id) {
  
  require(jsonlite)
  
  # Address
  x_link1 <- paste0("https://openapi.etsy.com/v2/users/",
                    user_id,
                    "/profile?api_key=",
                    api_key) 
  x_link2 <- gsub("/profile", "", x_link1)

  # Get data
  x1 <- fromJSON(x_link1) %>% .[["results"]]
  x2 <- fromJSON(x_link2) %>% .[["results"]]
  
  # Filter data
  x1 %<>%
    select(user_id, 
           country_id,
           region,
           city,
           transaction_sold_count
    )
  x2 %<>% .[["feedback_info"]] %>% 
    rename_all(~paste0("feedback_", .))
  
  # Merge
  x <- bind_cols(x1, x2) %>% 
    as_tibble()
  
  # Return results
  return(x)
  
}


# get.user.shops ----------------------------------------------------------

get.user.shops <- function(user_id) {
  
  require(jsonlite)
  
  # Address
  x_link <- paste0("https://openapi.etsy.com/v2/users/",
                    user_id,
                    "/shops?api_key=",
                    api_key) 
  
  # Get data
  x <- fromJSON(x_link) %>% .[["results"]] 
  
  # Filter data
  x %<>%
    select(user_id, 
           shop_id,
           num_favorers
    ) %>% 
    as_tibble()
  
  # Return results
  return(x)
  
}



# search.listings.id ------------------------------------------------------

search.listings.id <- function(search_key, min_price = 0, max_price = 50, start_page = 1, end_page = 10) {
  
  require(RSelenium)
  
  # Address
  x_link <- paste0("https://www.etsy.com/search?q=",
                   gsub(" ", "+", search_key),
                   "&min=", 
                   min_price,
                   "&max=", 
                   max_price,
                   "&order=most_relevant",
                   "&page=")
  
  # Initialize session
  rD <- rsDriver(
    port = 4586L,
    browser = "chrome",
    chromever = "92.0.4515.107"
  )
  remDr <- rD[["client"]] 
  
  # Get id in all pages
  x <- map(start_page:end_page, function(i) {
    
    remDr$navigate(paste0(x_link, i))
    Sys.sleep(runif(1, max = 3))
    remDr$findElements(using = "css selector", "[data-appears-component-name='search2_organic_listings_group']") %>% 
      map_chr(function(x_tmp) {
    
        x_tmp$getElementAttribute("data-appears-event-data") %>% 
          str_extract_all("[0-9]+") %>% 
          `[[`(1) %>% 
          `[`(2) 
        
      }) 
    }) %>% 
    flatten_chr() %>% 
    enframe("rank", "id") %>% 
    mutate(rank = rank + (start_page - 1) * 48)
  
  # Close connection
  remDr$close()
  rD$server$stop()
  rm(rD, remDr)
  gc()
  system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)
  
  # Return output
  return(x)
}


