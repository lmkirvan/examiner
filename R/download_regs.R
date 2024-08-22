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

single_page <- rvest::read_html(res[[1]][[1]])

# get children 
# remove expandables 
# dowload and folder cleaned html
# write retrieval function that works on the remaining xml 
# just provide the full citation to get the text 

nodeset <- single_page |>
  rvest::html_node("#content__main") |> 
  rvest::html_children() |> 
  rvest::html_elements("p") |> 
  rvest::html_attrs() 






