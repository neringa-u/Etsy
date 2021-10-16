library(magrittr)
library(tidyverse)
source("functions.R")

download <- F

api_key <- read_csv("api_key.txt", col_names = F) %>% pull(X1)
search_keys <- c("boho tassel earrings", "tassel earrings", "boho earrings", "earrings")

# Get search results
if (download) {
  
  all_data <- vector("list", length(search_keys))
  names(all_data) <- search_keys
  for (key in search_keys) {
    
    Sys.sleep(runif(1, 0, 3))
    
    search_data <- search.listings.id(key)
    search_data %<>% 
      mutate_at("id", as.numeric) %>% 
      left_join(
        map_df(search_data$id, function(id) get.listing(id)), 
        by = c("id" = "listing_id")
      )
    search_data %<>% 
      left_join(
        map_df(unique(search_data$user_id), function(id) get.user.profile(id))
      )
    all_data[[key]] <- search_data
  
  }

} else all_data <- readRDS("search_results_2021-09-03.rds")
  
# Subset and manipulate
x <- all_data[["boho earrings"]] %>% 
  as_tibble() %>% 
  select(
    rank, title, description, tags, materials, 
    original_creation_tsz, last_modified_tsz,
    views, num_favorers, 
    transaction_sold_count, feedback_count, feedback_score
  ) %>% 
  mutate(
    original_creation = Sys.time() - original_creation_tsz,
    original_creation = original_creation - min(original_creation) + 1,
    last_modified = Sys.time() - last_modified_tsz,
    last_modified = last_modified - min(last_modified) + 1,
    share_favorers = num_favorers/views,
    share_feedback = feedback_count/transaction_sold_count
  )
