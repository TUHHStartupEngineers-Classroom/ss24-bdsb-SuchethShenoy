library(rvest)
library(lubridate)

# Define the URL for the arXiv search page
topic <- "Koopman+Operator+Theory"  # Replace with the topic you're interested in
search_url <- paste0("https://arxiv.org/search/?query=", topic, "&searchtype=all&order=-announced_date_first&size=50")

# Read the HTML content of the search page
search_page <- read_html(search_url)

# Extract the URLs of the search results (papers)
paper_urls <- search_page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset(pattern = "abs")

papers_info <- tibble(
  url = character(),
  title = character(),
  abstract = character(),
  author_names = character(),
  submission_date = character()
)

for (url in paper_urls) {
  paper_page <- read_html(url)
  title <- paper_page %>%
    html_node("title") %>%
    html_text()
  
  abstract <- paper_page %>%
    html_node("meta[property='og:description']") %>%
    html_attr("content")
  
  author_names <- paper_page %>%
    html_node(".authors") %>%
    html_text() %>%
    gsub("Authors:", "", .) %>%
    trimws()
    
  submission_date <- paper_page %>%
    html_node(".dateline") %>%
    html_text() %>%
    gsub("\\[Submitted on |\\]", "", .) %>%
    trimws()
  
  papers_info <- papers_info %>%
    add_row(
      url = url,
      title = title,
      abstract = abstract,
      author_names = author_names,
      submission_date = submission_date
    )
}
