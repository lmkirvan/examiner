site <- "https://www.consumerfinance.gov/compliance/supervisory-highlights/"

# Function to download a file given a URL
download_file <- function(url, folder) {
  local_filename <- basename(url) |> stringr::str_replace_all("-", "_")
  local_filepath <- file.path(folder, local_filename)
  
  # Create directory if it does not exist
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  # Download the file
  GET(url, write_disk(local_filepath, overwrite = TRUE))
  message("Downloaded: ", local_filepath)
}

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

download_pdfs_from_page(site, "supervision/")

