# todo maybe should the missing values be empty strings here so that we can
# paste arbitrary columns together.

library(purrr)
library(dplyr)

make_frame <- function(rows, text, docs){
  if(length(rows) == 0){
    rows <- array(dim = c(1,7))
  }
  nr <- nrow(rows)
  array_branch(rows) |>
    flatten_chr()  |> 
    matrix(nrow =nr , ncol = 7, byrow = FALSE) |> 
    data.frame() -> df
  df$text = text
  df$doc = docs
  df
}

sections_re <- "[0-9]{1,2}\\.[0-9]{1,2}[[:alpha:] ]+\n"

volume_usc_re <- "([0-9]{1,2}[:space:]U[\\.]{0,1}S[\\.]{0,1}C[\\.]{0,1})"
section_usc_re <- "[\\s]{0,1}[§]{0,2}[\\s]{0,1}([0-9]{4}[ \\.,]{0,1}[0-9]{0,4}[a-z0-9\\-]{0,3})"
subsections_usc <- "(\\([a-z]{0,1}\\)){0,1}(\\([0-9]{0,2}\\)){0,1}(\\([A-Z]\\)){0,1}(\\([ivx]{0,10}\\)){0,1}"
usc_re <- paste0(volume_usc_re, section_usc_re, subsections_usc)

volume_cfr_re <- "([0-9]{1,2}[:space:]C[\\.]{0,1}F[\\.]{0,1}R[\\.]{0,1})"
section_cfr_re <- "[\\s]{0,1}[§]{0,2}[Part]{0,5}[\\,\\s]{0,1}([0-9]{4})[ \\.,]([0-9]{0,4}[a-z0-9\\-]{0,3})"
subsections_cfr_re <- "(\\([a-z]{0,1}\\)){0,1}(\\([0-9]{0,2}\\)){0,1}(\\([ivx]{0,10}\\)){0,1}"
cfr_re <- paste0(volume_cfr_re, section_cfr_re, subsections_cfr_re)

pdfs <- list.files("supervision/")

text <- map_chr(
  paste0("supervision/", pdfs) 
  ,  \(x) {pdftools::pdf_text(x) |> stringr::str_c(collapse = "")}
  )

text_sections <- map(text, stringr::str_split, pattern = sections_re)
names(text_sections) <- pdfs |> stringr::str_remove(".pdf")

cfrs <- map(
  text_sections, \(x){
    purrr::map(
      x
      , \(y) {
        stringr::str_match_all(y, cfr_re)
      }
    )
  }
) 

rows <- flatten(cfrs) |> flatten()
text <- flatten(text_sections) |> flatten()
lengths <- map_int(text_sections |>  flatten(), length)
docs <- rep(x = names(text_sections), lengths)

pmapper <- list(rows, text, docs)

result <- pmap(pmapper, make_frame) |> 
  reduce(dplyr::bind_rows)

names(result) <-c(
  "full"
  , "volume"
  , "section"
  , "sub1"
  , "sub2"
  , "sub3"
  , "sub4"
  , "text"
  , "doc"
  )

uscs <- map(
  text_sections, \(x){
    purrr::map(
      x
      , \(y) {
        stringr::str_match_all(y, usc_re)
      }
    )
  }
) 

rows_usc <- flatten(uscs) |> flatten()
pmapper <- list(rows_usc, text, docs)

result_usc <- pmap(pmapper, make_frame) |> 
  reduce(dplyr::bind_rows) 

names(result_usc) <-c(
    "full"
    , "volume"
    , "section"
    , "sub1"
    , "sub2"
    , "sub3"
    , "sub4"
    , "text"
    , "doc"
  )

result_usc <- result_usc |> 
  mutate(section = stringr::str_remove(section, "[ ]{0,1}and|et|\\.|,"))

result$source <- "CFR"
result_usc$source <- "USC"

final <- bind_rows(result, result_usc)

final <- final[, c(
  "doc"
  , "source"
  , "text"
  , "full"
  , "volume"
  , "section"
  , "sub1"
  , "sub2"
  , "sub3"
  , "sub4"
 )] |> 
  mutate(page = "sup")

readr::write_csv(final, "data/sup_citations.csv")
