library(purrr)

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

pdfs <- list.files("docs/")

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

names(result) <-c("full", "volume", "section", "dot_section", "alpha", "number", "roman", "text", "doc")

library(dplyr)

result |> 
  mutate(sect_dot = paste0(section,".", dot_section)) |> 
  group_by(sect_dot) |> 
  tally() -> temp


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

rows <- flatten(cfrs) |> flatten()
text <- flatten(text_sections) |> flatten()
lengths <- map_int(text_sections |>  flatten(), length)
docs <- rep(x = names(text_sections), lengths)


pmapper <- list(rows, text, docs)
result <- pmap(pmapper, make_frame) |> 
  reduce(dplyr::bind_rows)


write.csv(result, "sup_citations.csv")
