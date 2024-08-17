library(httr2)
library(rvest)
library(purrr)


base_url <- "https://www.consumerfinance.gov/rules-policy/regulations/"

return_links <- function(site_url, .p, base_url){
  
  site <- rvest::read_html(site_url)
  
  
  links_out <- site |> 
    rvest::html_nodes("a") |> 
    rvest::html_attr("href") |> 
    purrr::keep(\(x) grepl(.p, x)) |> 
    tail(n = -1) 
  
  paste0(base_url, links_out)
}

regs_pages <- return_links(
  base_url
  , "/regulations/"
  , "https://www.consumerfinance.gov"
  )

res <- list()
for(page in regs_pages){
  
res[[page]] <- return_links(
  page
  , "/[0-9]{4}/[0-9]{1,3}" 
  , "https://www.consumerfinance.gov"
  )  
}

res[[2]]
