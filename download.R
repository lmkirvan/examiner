site <- "https://www.consumerfinance.gov/compliance/supervisory-highlights/"

download.file(site, 'site.html')

text <- readLines('site.html') |> stringr::str_c(collapse = "\n") 

all_links <- stringr::str_match_all(text, "https://.+pdf") |> purrr::flatten_chr()

names <- stringr::str_remove(all_links, ".+documents/") |> 
  stringr::str_remove(".+/f/")  |> 
  stringr::str_replace_all("-","_" )

names <- paste0("docs/",names)

purrr::walk2(all_links, names,  \(x, y) download.file(url = x, destfile = y))

if(! "docs" %in% list.dirs()){
  system("mkdir docs")
}

system("mv *.pdf ./docs/")











