lex_relation_values <- c("cites", "amends", "repeals", "based_on", "implements")

lex_citations_prototype <- function() {
  tibble::tibble(
    from_id = character(),
    to_id = character(),
    relation = character(),
    to_version_date = as.Date(character()),
    source = character()
  )
}

#' Relations between legal documents in the lexverse standard form
#'
#' Creates a `lex_citations` table: one row per directed relation between
#' two legal instruments, identified by their lexverse identifiers.
#'
#' @section Columns:
#' \describe{
#'   \item{`from_id`}{[lex_id()] of the document making the reference.}
#'   \item{`to_id`}{[lex_id()] of the document referred to.}
#'   \item{`relation`}{One of `"cites"` (general reference), `"amends"`,
#'     `"repeals"`, `"based_on"` (issued under an enabling provision in
#'     `to_id`) or `"implements"` (transposes or implements `to_id`).}
#'   \item{`to_version_date`}{Date of the version of `to_id` the relation
#'     points at, where the source records it. Some sources update
#'     references to the current consolidated version, so this date can be
#'     later than the referring document itself.}
#'   \item{`source`}{Name of the package the data came from.}
#' }
#'
#' @param ... Name-value pairs of columns, passed to [tibble::tibble()].
#' @param x An object to test or validate.
#'
#' @return `lex_citations()` returns a `lex_citations` tibble.
#'   `validate_lex_citations()` returns `x` invisibly, or signals an error.
#'   `is_lex_citations()` returns `TRUE` or `FALSE`.
#' @export
#' @examples
#' lex_citations(
#'   from_id = "DK:B19980076305",
#'   to_id = "DK:A19990059229",
#'   relation = "based_on",
#'   to_version_date = as.Date("1999-07-14"),
#'   source = "danlex"
#' )
lex_citations <- function(...) {
  x <- fill_columns(tibble::tibble(...), lex_citations_prototype())
  x <- tibble::new_tibble(x, class = "lex_citations")
  validate_lex_citations(x)
  x
}

#' @rdname lex_citations
#' @export
validate_lex_citations <- function(x) {
  check_columns(x, lex_citations_prototype(), "lex_citations")

  for (nm in c("from_id", "to_id", "relation")) {
    if (anyNA(x[[nm]])) {
      cli::cli_abort("Column {.field {nm}} must not contain missing values.")
    }
  }
  ids <- c(x$from_id, x$to_id)
  bad_id <- unique(ids[!is_lex_id_format(ids)])
  if (length(bad_id) > 0) {
    cli::cli_abort("Not a valid lexverse identifier: {.val {bad_id}}.")
  }
  bad_r <- unique(x$relation[!x$relation %in% lex_relation_values])
  if (length(bad_r) > 0) {
    cli::cli_abort("Unknown {.field relation} value{?s}: {.val {bad_r}}.")
  }
  invisible(x)
}

#' @rdname lex_citations
#' @export
is_lex_citations <- function(x) inherits(x, "lex_citations")
