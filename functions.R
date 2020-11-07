# get.listing -------------------------------------------------------------

get.listing <- function(listing_id) {
  
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
           creation_tsz,
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
  
  # Return results
  return(x)
  
}



# get.listings ------------------------------------------------------------

get.listings <- function(search_key, min_price = 0, max_price = 50, limit = 100) {
  
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
           creation_tsz,
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
  
  # Return results
  return(x)
}


# get.user.profile --------------------------------------------------------

get.user.profile <- function(user_id) {
  
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



