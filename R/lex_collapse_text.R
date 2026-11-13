#' Combine provisions into whole-document text
#'
#' Joins the provisions of each document in a [lex_texts()] table into one
#' text, in document order. Each language version is collapsed separately.
#'
#' @param x A [lex_texts()] table.
#' @param sep String placed between provisions.
#' @param drop_amendments If `TRUE`, provisions marked as new text destined
#'   for another instrument (`is_amendment`) are left out, so that only the
#'   document's own provisions remain.
#'
#' @return A [lex_texts()] table with one row per document and language,
#'   `provision_index` 1 and `section` missing.
#' @export
#' @examples
#' x <- lex_texts(
#'   lex_id = "FI:2025/100", language = "fi",
#'   provision_index = 1:2, section = c("1", "2"),
#'   text = c("Ensimm\u00e4inen pyk\u00e4l\u00e4.", "Toinen pyk\u00e4l\u00e4."),
#'   source = "finlex"
#' )
#' lex_collapse_text(x)
lex_collapse_text <- function(x, sep = "\n\n", drop_amendments = FALSE) {
  if (!is_lex_texts(x)) {
    cli::cli_abort("{.arg x} must be a {.cls lex_texts} table.")
  }
  if (isTRUE(drop_amendments)) {
    x <- x[is.na(x$is_amendment) | !x$is_amendment, , drop = FALSE]
  }
  x <- x[order(x$lex_id, x$language, x$provision_index), , drop = FALSE]

  key <- paste(x$lex_id, x$language, sep = "\r")
  groups <- split(seq_len(nrow(x)), factor(key, levels = unique(key)))
  first <- vapply(groups, function(i) i[1], integer(1))
  text <- vapply(groups, function(i) {
    t <- x$text[i]
    paste(t[!is.na(t)], collapse = sep)
  }, character(1))

  lex_texts(
    lex_id = x$lex_id[first],
    language = x$language[first],
    provision_index = rep(1L, length(groups)),
    text = unname(text),
    source = x$source[first]
  )
}
