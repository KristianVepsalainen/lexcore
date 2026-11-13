# Synthetic fixtures for provision-level text output.

fixture_finlex_text <- function() {
  tibble::tibble(
    year = 2025L,
    number = 100L,
    status = "ok",
    section_index = 1:2,
    section_id = c("sec_1", "sec_3a"),
    heading = c("Soveltamisala", "Voimaantulo"),
    text = c(
      "1 \u00a7SoveltamisalaT\u00e4m\u00e4 asetus koskee suoritteita.",
      "3 a \u00a7VoimaantuloT\u00e4m\u00e4 asetus tulee voimaan."
    )
  )
}

fixture_danlex_paragraphs <- function() {
  tibble::tibble(
    eli = "https://retsinformation.dk/eli/lta/2025/50",
    paragraf_index = c(1L, 1L, 2L),
    paragraf_id = c("id1", "id1", "id2"),
    paragraf_local_id = c("1", "1", "2"),
    paragraf = c("\u00a7 1.", "\u00a7 1.", "\u00a7 2."),
    stk_index = c(1L, 2L, 1L),
    stk = c(NA, "Stk. 2.", NA),
    is_amendment = FALSE,
    text = c("Form\u00e5let er ...", "Den uddannede ...", "Uddannelsen ...")
  )
}

fixture_danlex_docs_for_texts <- function() {
  lex_documents(
    jurisdiction = "DK",
    doc_id = "B20250005005",
    eli = "https://retsinformation.dk/eli/lta/2025/50",
    source = "danlex"
  )
}

fixture_eurlex_text <- function() {
  c(html = "15.4.2010 EN Official Journal of the European Union L 95/1\n\nDIRECTIVE 2010/13/EU")
}
