lex_status_values <- c("ok", "not_found", "metadata_only", "pdf_only", "error")

lex_documents_prototype <- function() {
  tibble::tibble(
    lex_id = character(),
    jurisdiction = character(),
    doc_id = character(),
    eli = character(),
    doc_type = character(),
    doc_type_native = character(),
    title = character(),
    language = character(),
    date_document = as.Date(character()),
    date_in_force = as.Date(character()),
    in_force = logical(),
    status = character(),
    source = character(),
    source_url = character()
  )
}

#' Legal documents in the lexverse standard form
#'
#' Creates a `lex_documents` table: one row per legal instrument, with the
#' same columns whichever jurisdiction or package the data came from.
#' Columns not supplied are filled with missing values of the correct type,
#' and `lex_id` is built from `jurisdiction` and `doc_id` when not given.
#' Extra columns are kept after the standard ones.
#'
#' @section Columns:
#' \describe{
#'   \item{`lex_id`}{Unique lexverse identifier; see [lex_id()].}
#'   \item{`jurisdiction`}{Two-letter code, `"EU"` for the European Union.}
#'   \item{`doc_id`}{The source's own identifier.}
#'   \item{`eli`}{European Legislation Identifier, where available.}
#'   \item{`doc_type`}{Harmonised document type.}
#'   \item{`doc_type_native`}{Document type as the source records it.}
#'   \item{`title`}{Official title.}
#'   \item{`language`}{ISO 639-1 language code of the title.}
#'   \item{`date_document`}{Date of signature or adoption.}
#'   \item{`date_in_force`}{Date of entry into force.}
#'   \item{`in_force`}{Whether the instrument is currently in force.}
#'   \item{`status`}{Retrieval status: `"ok"`, `"not_found"`,
#'     `"metadata_only"`, `"pdf_only"` or `"error"`.}
#'   \item{`source`}{Name of the package the data came from.}
#'   \item{`source_url`}{URL of the record at the source.}
#' }
#'
#' @param ... Name-value pairs of columns, passed to [tibble::tibble()].
#' @param x An object to test or validate.
#'
#' @return `lex_documents()` returns a `lex_documents` tibble.
#'   `validate_lex_documents()` returns `x` invisibly, or signals an error.
#'   `is_lex_documents()` returns `TRUE` or `FALSE`.
#' @export
#' @examples
#' lex_documents(
#'   jurisdiction = "FI",
#'   doc_id = "2025/100",
#'   title = "Valtioneuvoston asetus",
#'   date_document = as.Date("2025-03-18"),
#'   status = "ok",
#'   source = "finlex"
#' )
lex_documents <- function(...) {
  x <- fill_columns(tibble::tibble(...), lex_documents_prototype())
  if (all(is.na(x$lex_id))) {
    x$lex_id <- lex_id(x$jurisdiction, x$doc_id)
  }
  x <- new_lex_documents(x)
  validate_lex_documents(x)
  x
}

new_lex_documents <- function(x) {
  stopifnot(is.data.frame(x))
  tibble::new_tibble(x, class = "lex_documents")
}

#' @rdname lex_documents
#' @export
validate_lex_documents <- function(x) {
  check_columns(x, lex_documents_prototype(), "lex_documents")

  if (anyNA(x$lex_id)) {
    cli::cli_abort("Column {.field lex_id} must not contain missing values.")
  }
  dup <- unique(x$lex_id[duplicated(x$lex_id)])
  if (length(dup) > 0) {
    cli::cli_abort(
      "Column {.field lex_id} must be unique. Duplicated: {.val {dup}}."
    )
  }
  bad_j <- unique(x$jurisdiction[is.na(x$jurisdiction) |
                                   !is_jurisdiction(x$jurisdiction)])
  if (length(bad_j) > 0) {
    cli::cli_abort("Invalid {.field jurisdiction} value{?s}: {.val {bad_j}}.")
  }
  bad_s <- unique(x$status[!is.na(x$status) &
                             !x$status %in% lex_status_values])
  if (length(bad_s) > 0) {
    cli::cli_abort("Unknown {.field status} value{?s}: {.val {bad_s}}.")
  }
  invisible(x)
}

#' @rdname lex_documents
#' @export
is_lex_documents <- function(x) inherits(x, "lex_documents")
