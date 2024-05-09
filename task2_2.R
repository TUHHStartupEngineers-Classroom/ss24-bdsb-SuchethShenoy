library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs

# 1.2 COLLECT PRODUCT CATEGORIES ----

url_home        <- "https://www.radon-bikes.de/en"

# Read HTML for the entire webpage
html_home       <- read_html(url_home)

bike_categories_chr <- html_home %>%
  html_elements(".megamenu a") %>%
  html_attr("href") %>%
  str_subset(pattern = "#|wear", negate = T) %>%
  unique() %>%
  str_c("https://www.radon-bikes.de", ., sep = "")


get_bike_urls <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_chr  <- html_bike_category %>%
    html_elements("a.a-button") %>%
    html_attr("href") %>%
    str_subset(pattern = "bikegrid|slush|instagram", negate = T) %>%
    unique() %>%
    str_c("https://www.radon-bikes.de", ., sep = "")
  
  return(bike_url_chr)
  
}

bike_urls_chr <- map(bike_categories_chr, get_bike_urls) |>
  flatten_chr() |>
  unique()

bike_urls_tbl <- bike_urls_chr |>
  tibble::as_tibble_col(column_name = "url") |>
  tidyr::separate_wider_regex(cols = url, patterns = c(".*de/en/", family   = "[^/]*", "/",
                                                       category = "[^/]*", "/",
                                                       model_type    = "[^/]*", "/",
                                                       model = "[^/]*", "/",
                                                       ".*"), cols_remove = F)

bike_urls_hardtail_tbl <- bike_urls_tbl |>
  filter(category == "hardtail")

get_model_data <- function(url) {
  
  html_bike_model <- read_html(url)
  
  bike_price <- html_bike_model |>
    html_element(".m-bikedetail__price--active") |>
    html_text() |>
    parse_number()
  
  bike_data <- tibble(url   = url,
                      price = bike_price)
  
  return(bike_data)
  
}

bike_model_data_tbl <- bike_urls_hardtail_tbl$url |> map_dfr(get_model_data)

bike_model_data_joined_tbl <- bike_urls_hardtail_tbl |> 
  left_join(bike_model_data_tbl, by = join_by("url"))