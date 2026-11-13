#' Convert data package output to the lexverse standard form
#'
#' Turns the output of a lexverse data package into a [lex_documents()]
#' table. The converters read column names only and do not call the source
#' packages, so none of them needs to be installed.
#'
#' Rows without an identifier, typically `"not_found"` results, are dropped
#' with a warning. For `eurlex`, which returns one row per value of any
#' multi-valued field, documents are deduplicated by CELEX number.
#'
#' Supported inputs:
#' * `"danlex"`: output of `dlx_get_doc()` or `dlx_get_docs()`.
#' * `"finlex"`: output of `flx_get_metadata()`.
#' * `"eurlex"`: output of `elx_run_query()`.
#'
#' @param x A data frame returned by a lexverse data package.
#' @param source Name of the package that produced `x`.
#'
#' @return A [lex_documents()] tibble.
#' @export
#' @examples
#' finlex_output <- data.frame(
#'   year = 2025L, number = 100L, status = "ok",
#'   date_issued = as.Date("2025-03-18"), title = "Asetus"
#' )
#' as_lex_documents(finlex_output, source = "finlex")
as_lex_documents <- function(x, source = c("danlex", "finlex", "eurlex")) {
  source <- rlang::arg_match(source)
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame.")
  }
  switch(source,
    danlex = docs_from_danlex(x),
    finlex = docs_from_finlex(x),
    eurlex = docs_from_eurlex(x)
  )
}

docs_from_danlex <- function(x) {
  require_cols(x, c("accn", "eli", "title", "document_type_std",
                    "dies_signi", "start_date", "legal_status", "status"),
               "danlex")
  x <- drop_unidentified(x, is.na(x$accn))
  n <- nrow(x)
  lex_documents(
    jurisdiction = rep("DK", n),
    doc_id = as.character(x$accn),
    eli = as.character(x$eli),
    doc_type = harmonise_doc_type(x$document_type_std, "danlex"),
    doc_type_native = as.character(x$document_type_std),
    title = as.character(x$title),
    language = rep("da", n),
    date_document = as.Date(x$dies_signi),
    date_in_force = as.Date(x$start_date),
    in_force = ifelse(x$legal_status == "Valid", TRUE,
                      ifelse(x$legal_status == "Historic", FALSE, NA)),
    status = as.character(x$status),
    source = rep("danlex", n),
    source_url = as.character(x$eli)
  )
}

docs_from_finlex <- function(x) {
  require_cols(x, c("year", "number", "status", "date_issued", "title"),
               "finlex")
  x <- drop_unidentified(x, is.na(x$year) | is.na(x$number))
  n <- nrow(x)
  lex_documents(
    jurisdiction = rep("FI", n),
    doc_id = paste(x$year, x$number, sep = "/"),
    title = as.character(x$title),
    language = rep("fi", n),
    date_document = as.Date(x$date_issued),
    status = as.character(x$status),
    source = rep("finlex", n)
  )
}

docs_from_eurlex <- function(x) {
  require_cols(x, "celex", "eurlex")
  x <- drop_unidentified(x, is.na(x$celex))
  x <- x[!duplicated(x$celex), , drop = FALSE]
  n <- nrow(x)
  col <- function(nm, f = as.character) {
    if (nm %in% names(x)) f(x[[nm]]) else rep(f(NA), n)
  }
  title <- col("title")
  lex_documents(
    jurisdiction = rep("EU", n),
    doc_id = as.character(x$celex),
    doc_type = harmonise_doc_type(col("type"), "eurlex"),
    doc_type_native = col("type"),
    title = title,
    language = ifelse(is.na(title), NA_character_, "en"),
    date_document = col("date", as.Date),
    in_force = col("force", as.logical),
    status = rep("ok", n),
    source = rep("eurlex", n),
    source_url = paste0(
      "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:", x$celex
    )
  )
}
