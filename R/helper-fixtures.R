# Synthetic fixtures mirroring the output of the lexverse data packages.
# testthat loads helper-*.R files automatically before the tests run.

fixture_danlex_doc <- function() {
  tibble::tibble(
    status = c("metadata_only", "not_found"),
    eli = c("https://retsinformation.dk/eli/lta/1998/763", NA),
    accn = c("B19980076305", NA),
    title = c("Bekendtg\u00f8relse om indbetaling", NA),
    document_type_std = c("Bekendtg\u00f8relse", NA),
    legal_status = c("Historic", NA),
    dies_signi = as.Date(c("1998-10-23", NA)),
    start_date = as.Date(c("1998-11-02", NA))
  )
}

fixture_danlex_refs <- function() {
  tibble::tibble(
    from_eli = "https://retsinformation.dk/eli/lta/1998/763",
    from_accn = "B19980076305",
    ref_index = 1:2,
    ref_accn = c("A19990059229", "A19990066629"),
    ref_date = as.Date(c("1999-07-14", "1999-08-19")),
    ref_title = c("Lov A", "Lov B")
  )
}

fixture_finlex_meta <- function() {
  tibble::tibble(
    year = 2025L, number = 100L, status = "ok",
    date_issued = as.Date("2025-03-18"),
    title = "Ty\u00f6- ja elinkeinoministeri\u00f6n asetus",
    n_sections = 4L
  )
}

fixture_finlex_affected <- function() {
  tibble::tibble(
    source_year = c(2023L, 2025L),
    source_number = c(2L, 100L),
    status = c("ok", "no_affected"),
    target_href = c("/akn/fi/act/statute/2015/1352", NA),
    target_year = c(2015L, NA),
    target_number = c(1352L, NA)
  )
}

fixture_eurlex <- function() {
  data.frame(
    work = c("w1", "w1", "w2"),
    type = "DIR",
    celex = c("32010L0013", "32010L0013", "32007L0002"),
    date = c("2010-03-10", "2010-03-10", "2007-03-14"),
    force = "true",
    citationcelex = c("52004IP0373", "32003R2152", NA)
  )
}
