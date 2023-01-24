# ------------------------------------------------------------------------------
# GNVR --------------------------------------+----------------------------------
# A Global Name Verifier R interface         |
# Isacco Darini, 18.10.2022                  |
# -------------------------------------------+----------------------------------
#
# Provides functions to apply the powerful Global Name Verifier 
# to a tibble using its API.
# GNVR is powerful and can recognize scientific names under many forms
# even if slightly misspelled.
# Currently limited to default settings.
# The batch function allows to overcome the URL length limit when checking
# more than 1000 names at once.
# 
# To do:
#  - Add options
#  - Streamline into one function
#
# DEPENDENCIES -----------------------------------------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(tibble)

# FUNCTIONS ---------------------------------------------------------------------

gnv_verify <- function(to_check){
  
  uniques <- unique(to_check)
  uniques_str <- 
    paste0(uniques, collapse = "|") %>% 
    stringr::str_replace_all(" ", "+")
  
  response <- httr::GET("https://verifier.globalnames.org/",
                        path = paste0("api/v1/verifications/", uniques_str))
  
  parsed <- jsonlite::fromJSON(content(response, "text"))
  
  selected <- 
    parsed["names"] %>%
    tibble::as_tibble() %>%
    tidyr::unnest_wider(names) %>%
    dplyr::select(name, bestResult) %>%
    tidyr::unnest_wider(bestResult) %>%
    dplyr::select(name, currentCanonicalFull)
  
  to_check %>% 
    as_tibble() %>% 
    left_join(., selected, by = c("value" = "name")) %>% 
    pull(currentCanonicalFull)
  
}

# Linux and Windows don't have the same behavior
# somewhere in the httr::GET() call on Windows the spaces are converted to "+".
# on Linux this doesn't happens therefore str_replace_all() is used to do the 
# same before passing the string to GET().

gnv_verify_batch <- function(to_check) {
  
  as_tibble_col(to_check, column_name = "to_check") %>%
    mutate(batch = ceiling(row_number()/1000)) %>% 
    group_by(batch) %>%
    group_split() %>% 
    map(~ mutate(.x, verified = gnv_verify(to_check))) %>% 
    bind_rows() %>% 
    pull(verified)
}

# EXAMPLES ---------------------------------------------------------------------

# Toy data
name_test <- c("Hyalosphenia papilio", "Archerella flabum", 
               "Big chungus", "", "Trigonopixis arcula", 
               NA_character_ , "Hyalosphenia papilio")

# On a vector
gnv_verify(name_test)

# On a tibble in a pipe
name_test_tb <- 
  as_tibble(name_test) %>% 
  mutate(verified = gnv_verify(value))

# On a tibble with batch processing

name_test_tb_batch <- 
  as_tibble(name_test) %>% 
  mutate(verified = gnv_verify_batch(value))

