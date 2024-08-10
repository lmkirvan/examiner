library(rvest)
library(httr)
library(stringr)

# Function to download a file given a URL
download_file <- function(url, folder) {
  local_filename <- basename(url)
  local_filepath <- file.path(folder, local_filename)
  
  # Create directory if it does not exist
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  # Download the file
  GET(url, write_disk(local_filepath, overwrite = TRUE))
  message("Downloaded: ", local_filepath)
}

# Function to find and download all PDFs from a webpage
download_pdfs_from_page <- function(page_url, download_folder) {
  # Read the webpage
  page <- read_html(page_url)
  
  # Extract PDF links
  pdf_links <- page |>
    html_nodes("a") |>
    html_attr("href") |>
    str_subset("\\.pdf$")
  
  # Create absolute URLs
  pdf_links <- url_absolute(pdf_links, base = page_url)
  
  # Download each PDF
  for (pdf_link in pdf_links) {
    download_file(pdf_link, download_folder)
  }
}

nav_pages <- paste0("https://www.consumerfinance.gov/enforcement/actions/?page=",1:15)

get_case_urls <- function(url){
  # Read the webpage
  page <- read_html(url)
  
  # Extract PDF links
  case_links <- page |>
    html_nodes("a") |>
    html_attr("href") |>
    str_subset("/enforcement/actions/.+")
  
  # Create absolute URLs
  case_links <- url_absolute(case_links, base = "https://www.consumerfinance.gov")
  case_links |> 
    discard(
      \(x) x == "https://www.consumerfinance.gov/enforcement/actions/enforcement-action-definitions/")
}


case_links <- map(nav_pages, get_case_urls) |> flatten_chr()

for(page in case_links){
  
  name <- stringr::str_remove(page, ".+/enforcement/actions/") |> 
    stringr::str_replace_all("-", "_")
  
  dir <-paste0("enforcement/", name)
  
  if(!dir.exists(dir)){
    dir.create(dir)
  }
  download_pdfs_from_page(page, download_folder = dir)
}
