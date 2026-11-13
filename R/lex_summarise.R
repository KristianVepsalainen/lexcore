#' Count legal documents by jurisdiction, year and type
#'
#' Summarises legislative output in a [lex_documents()] table. The year is
#' taken from `date_document`.
#'
#' @param x A [lex_documents()] table.
#' @param by Columns to group by: any of `"jurisdiction"`, `"year"`,
#'   `"doc_type"`, `"source"` and `"in_force"`.
#'
#' @return A tibble with the grouping columns and `n`, the number of
#'   documents, sorted by the grouping columns.
#' @export
#' @examples
#' x <- lex_documents(
#'   jurisdiction = c("FI", "FI", "DK"),
#'   doc_id = c("2025/1", "2025/2", "B2025001"),
#'   date_document = as.Date(c("2025-01-10", "2025-02-01", "2025-03-05"))
#' )
#' lex_summarise_output(x)
lex_summarise_output <- function(x, by = c("jurisdiction", "year")) {
  if (!is_lex_documents(x)) {
    cli::cli_abort("{.arg x} must be a {.cls lex_documents} table.")
  }
  allowed <- c("jurisdiction", "year", "doc_type", "source", "in_force")
  bad <- setdiff(by, allowed)
  if (length(by) == 0 || length(bad) > 0) {
    cli::cli_abort(c(
      "{.arg by} must be one or more of {.val {allowed}}.",
      "x" = "Unknown: {.val {bad}}."
    ))
  }
  d <- data.frame(
    jurisdiction = x$jurisdiction,
    year = as.integer(format(x$date_document, "%Y")),
    doc_type = x$doc_type,
    source = x$source,
    in_force = x$in_force
  )[unique(by)]

  count_groups(d)
}

#' Summarise relations between legal documents
#'
#' Describes a [lex_citations()] table either per document (how many
#' relations each document makes and receives, and how many of them cross
#' a jurisdictional border) or per pair of jurisdictions. Network analysis
#' proper belongs to the lexnet package; this function gives the
#' descriptive overview.
#'
#' @param x A [lex_citations()] table.
#' @param level `"document"` or `"jurisdiction"`.
#' @param documents For `level = "document"` only: an optional
#'   [lex_documents()] table. If given, the result has one row per document
#'   in it, including those with no relations.
#'
#' @return For `level = "document"`, a tibble with `lex_id`, `n_out`,
#'   `n_in`, `n_out_foreign` and `n_in_foreign`. For
#'   `level = "jurisdiction"`, a tibble with `from_jurisdiction`,
#'   `to_jurisdiction`, `relation` and `n`.
#' @export
#' @examples
#' x <- lex_citations(
#'   from_id = c("FI:2023/2", "FI:2023/2", "DK:B1998"),
#'   to_id = c("FI:2015/1352", "EU:32010L0013", "DK:A1999"),
#'   relation = c("amends", "implements", "based_on")
#' )
#' lex_summarise_citations(x)
#' lex_summarise_citations(x, level = "jurisdiction")
lex_summarise_citations <- function(x, level = c("document", "jurisdiction"),
                                    documents = NULL) {
  if (!is_lex_citations(x)) {
    cli::cli_abort("{.arg x} must be a {.cls lex_citations} table.")
  }
  level <- rlang::arg_match(level)
  from_j <- parse_lex_id(x$from_id)$jurisdiction
  to_j <- parse_lex_id(x$to_id)$jurisdiction

  if (level == "jurisdiction") {
    if (!is.null(documents)) {
      cli::cli_abort("{.arg documents} applies to {.code level = \"document\"} only.")
    }
    d <- data.frame(
      from_jurisdiction = from_j,
      to_jurisdiction = to_j,
      relation = x$relation
    )
    return(count_groups(d))
  }

  if (is.null(documents)) {
    ids <- unique(c(x$from_id, x$to_id))
  } else {
    if (!is_lex_documents(documents)) {
      cli::cli_abort("{.arg documents} must be a {.cls lex_documents} table.")
    }
    ids <- documents$lex_id
  }
  foreign <- from_j != to_j
  count <- function(v, keep = rep(TRUE, length(v))) {
    as.integer(table(factor(v[keep], levels = ids)))
  }
  tibble::tibble(
    lex_id = ids,
    n_out = count(x$from_id),
    n_in = count(x$to_id),
    n_out_foreign = count(x$from_id, foreign),
    n_in_foreign = count(x$to_id, foreign)
  )
}

count_groups <- function(d) {
  key <- do.call(paste, c(unname(as.list(d)), sep = "\r"))
  levels <- unique(key)
  out <- d[!duplicated(key), , drop = FALSE]
  out$n <- as.integer(table(factor(key, levels = levels)))
  if (nrow(out) > 0) {
    out <- out[do.call(order, unname(as.list(out[names(d)]))), , drop = FALSE]
  }
  tibble::as_tibble(out)
}
