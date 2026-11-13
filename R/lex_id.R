#' Build and parse lexverse identifiers
#'
#' A lexverse identifier joins a jurisdiction code and the source's own
#' document identifier with a colon, for example `"FI:2025/100"`,
#' `"DK:B19980076305"` or `"EU:32010L0013"`. Only the first colon separates
#' the two parts, so source identifiers may themselves contain colons, as
#' Swedish SFS numbers such as `"1974:152"` do.
#'
#' @param jurisdiction Character vector of two-letter upper-case codes:
#'   ISO 3166-1 alpha-2 for countries and `"EU"` for the European Union.
#' @param id Character vector of source identifiers.
#' @param x Character vector of lexverse identifiers.
#'
#' @return `lex_id()` returns a character vector, with `NA` wherever either
#'   input is missing. `parse_lex_id()` returns a tibble with columns
#'   `jurisdiction` and `doc_id`.
#' @export
#' @examples
#' lex_id("FI", "2025/100")
#' lex_id(c("SE", "EU"), c("1974:152", "32010L0013"))
#' parse_lex_id("SE:1974:152")
lex_id <- function(jurisdiction, id) {
  jurisdiction <- as.character(jurisdiction)
  id <- as.character(id)
  bad <- !is.na(jurisdiction) & !is_jurisdiction(jurisdiction)
  if (any(bad)) {
    cli::cli_abort(
      "Invalid jurisdiction code{?s}: {.val {unique(jurisdiction[bad])}}."
    )
  }
  out <- paste0(jurisdiction, ":", id)
  out[is.na(jurisdiction) | is.na(id)] <- NA_character_
  out
}

#' @rdname lex_id
#' @export
parse_lex_id <- function(x) {
  x <- as.character(x)
  ok <- is.na(x) | is_lex_id_format(x)
  if (!all(ok)) {
    cli::cli_abort(
      "Not a valid lexverse identifier: {.val {unique(x[!ok])}}."
    )
  }
  tibble::tibble(
    jurisdiction = ifelse(is.na(x), NA_character_, sub(":.*$", "", x)),
    doc_id = ifelse(is.na(x), NA_character_, sub("^[^:]*:", "", x))
  )
}
