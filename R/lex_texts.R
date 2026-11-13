lex_texts_prototype <- function() {
  tibble::tibble(
    lex_id = character(),
    language = character(),
    provision_index = integer(),
    section = character(),
    subsection = character(),
    section_native = character(),
    heading = character(),
    is_amendment = logical(),
    text = character(),
    source = character()
  )
}

#' Legal texts at provision level in the lexverse standard form
#'
#' Creates a `lex_texts` table: one row per provision (section, or
#' subsection where the source distinguishes them), linked to
#' [lex_documents()] through `lex_id`. Whole-document text is stored as a
#' single row with `section` missing.
#'
#' The key is `lex_id`, `language` and `provision_index` together, since
#' one instrument may exist in several languages.
#'
#' @section Columns:
#' \describe{
#'   \item{`lex_id`}{[lex_id()] of the document the provision belongs to.}
#'   \item{`language`}{ISO 639-1 language code of the text.}
#'   \item{`provision_index`}{Position of the provision in document order.}
#'   \item{`section`}{Normalised section number, such as `"1"` or `"3 a"`;
#'     see [normalise_section()].}
#'   \item{`subsection`}{Subsection number, where the source records it.}
#'   \item{`section_native`}{Section label as the source writes it.}
#'   \item{`heading`}{Section heading, where the source has one.}
#'   \item{`is_amendment`}{Whether the provision is new text destined for
#'     another instrument rather than the document's own.}
#'   \item{`text`}{Text of the provision.}
#'   \item{`source`}{Name of the package the data came from.}
#' }
#'
#' @param ... Name-value pairs of columns, passed to [tibble::tibble()].
#' @param x An object to test or validate.
#'
#' @return `lex_texts()` returns a `lex_texts` tibble.
#'   `validate_lex_texts()` returns `x` invisibly, or signals an error.
#'   `is_lex_texts()` returns `TRUE` or `FALSE`.
#' @export
#' @examples
#' lex_texts(
#'   lex_id = "FI:2025/100",
#'   language = "fi",
#'   provision_index = 1:2,
#'   section = c("1", "2"),
#'   text = c("Soveltamisala ...", "Maksulliset suoritteet ..."),
#'   source = "finlex"
#' )
lex_texts <- function(...) {
  x <- fill_columns(tibble::tibble(...), lex_texts_prototype())
  x <- tibble::new_tibble(x, class = "lex_texts")
  validate_lex_texts(x)
  x
}

#' @rdname lex_texts
#' @export
validate_lex_texts <- function(x) {
  check_columns(x, lex_texts_prototype(), "lex_texts")

  for (nm in c("lex_id", "provision_index")) {
    if (anyNA(x[[nm]])) {
      cli::cli_abort("Column {.field {nm}} must not contain missing values.")
    }
  }
  bad_id <- unique(x$lex_id[!is_lex_id_format(x$lex_id)])
  if (length(bad_id) > 0) {
    cli::cli_abort("Not a valid lexverse identifier: {.val {bad_id}}.")
  }
  key <- paste(x$lex_id, x$language, x$provision_index, sep = "\r")
  if (anyDuplicated(key) > 0) {
    cli::cli_abort(c(
      "Each provision must appear once.",
      "x" = "{.field lex_id}, {.field language} and {.field provision_index} together contain duplicates."
    ))
  }
  invisible(x)
}

#' @rdname lex_texts
#' @export
is_lex_texts <- function(x) inherits(x, "lex_texts")

#' Normalise section labels across jurisdictions
#'
#' Reduces section labels written in different national conventions to a
#' common form, so that provisions can be compared across jurisdictions.
#' The section sign, the words for "article", trailing full stops and extra
#' whitespace are removed, and a letter suffix is separated by one space.
#'
#' @param x Character vector of section labels.
#'
#' @return A character vector of the same length; `NA` where nothing
#'   remains.
#' @export
#' @examples
#' normalise_section(c("1 \u00a7", "\u00a7 1.", "Article 12", "3a \u00a7"))
normalise_section <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00a7|Article|Artikel|Artikla|\\.", "", x, ignore.case = TRUE)
  x <- trimws(gsub("\\s+", " ", x))
  x <- sub("^(\\d+)\\s*([[:alpha:]])$", "\\1 \\2", x)
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}
