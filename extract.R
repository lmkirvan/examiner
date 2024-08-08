text <- pdftools::pdf_text(
  "docs/cfpb_supervisory_highlights_issue_24_2021_06.pdf"
  ) |> 
  stringr::str_c(collapse = "") 

sections_re <- "[0-9]{1,2}\\.[0-9]{1,2}[[:alpha:] ]+\n"
sections <- text |> stringr::str_split(product_sections_re)

volume_usc <- "([0-9]{1,2}[:space:]U[\\.]{0,1}S[\\.]{0,1}C[\\.]{0,1})"
section_usc <- "[\\s]{0,1}[§]{0,2}[\\s]{0,1}([0-9]{4}[ \\.,]{0,1}[0-9]{0,4}[a-z0-9\\-]{0,3})"
subsections_usc <- "(\\([a-z]{0,1}\\)){0,1}(\\([0-9]{0,2}\\)){0,1}(\\([A-Z]\\)){0,1}(\\([ivx]{0,10}\\)){0,1}"

volume_cfr <- "([0-9]{1,2}[:space:]C[\\.]{0,1}F[\\.]{0,1}R[\\.]{0,1})"
section_cfr <- "[\\s]{0,1}[§]{0,2}[Part]{0,5}[\\,\\s]{0,1}([0-9]{4})[ \\.,]([0-9]{0,4}[a-z0-9\\-]{0,3})"
subsections_cfr <- "(\\([a-z]{0,1}\\)){0,1}(\\([0-9]{0,2}\\)){0,1}(\\([ivx]{0,10}\\)){0,1}"

stringr::str_match_all(text, paste0(volume_cfr, section_cfr, subsections_cfr))

sections <- stringr::str_split(text, "[0-9]{1,2}\\.[0-9]{1,2}[[:alpha:] ]+\n")

purrr::map(sections, \(x) stringr::str_match_all(x, paste0(volume_cfr, section_cfr, subsections_cfr))) -> temp







