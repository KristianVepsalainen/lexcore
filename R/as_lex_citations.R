#' Convert relations from data package output to the lexverse standard form
#'
#' Turns the relation output of a lexverse data package into a
#' [lex_citations()] table. Like [as_lex_documents()], the converters read
#' column names only and do not call the source packages.
#'
#' Supported inputs:
#' * `"danlex"`: output of `dlx_get_references()`. These are enabling
#'   provisions, recorded as `relation = "based_on"`.
#' * `"finlex"`: output of `flx_get_affected()`. The source does not say
#'   whether a statute amends or repeals its target, so `relation` must
#'   be supplied; the statute type from `flx_download_statutes()` tells
#'   which applies.
#' * `"eurlex"`: output of `elx_run_query()` with `include_citations = TRUE`,
#'   recorded as `relation = "cites"`.
#'
#' Rows without a target are dropped, and duplicated relations are kept once.
#'
#' @inheritParams as_lex_documents
#' @param relation For `source = "finlex"` only: `"amends"` or `"repeals"`.
#'
#' @return A [lex_citations()] tibble.
#' @export
#' @examples
#' danlex_output <- data.frame(
#'   from_accn = "B19980076305", ref_accn = "A19990059229",
#'   ref_date = as.Date("1999-07-14")
#' )
#' as_lex_citations(danlex_output, source = "danlex")
as_lex_citations <- function(x, source = c("danlex", "finlex", "eurlex"),
                             relation = NULL) {
  source <- rlang::arg_match(source)
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame.")
  }
  if (!is.null(relation) && source != "finlex") {
    cli::cli_abort("{.arg relation} applies to {.val finlex} input only.")
  }
  out <- switch(source,
    danlex = cites_from_danlex(x),
    finlex = cites_from_finlex(x, relation),
    eurlex = cites_from_eurlex(x)
  )
  out[!duplicated(out[c("from_id", "to_id", "relation")]), , drop = FALSE]
}

cites_from_danlex <- function(x) {
  require_cols(x, c("from_accn", "ref_accn", "ref_date"), "danlex")
  x <- x[!is.na(x$from_accn) & !is.na(x$ref_accn), , drop = FALSE]
  n <- nrow(x)
  lex_citations(
    from_id = lex_id(rep("DK", n), x$from_accn),
    to_id = lex_id(rep("DK", n), x$ref_accn),
    relation = rep("based_on", n),
    to_version_date = as.Date(x$ref_date),
    source = rep("danlex", n)
  )
}

cites_from_finlex <- function(x, relation) {
  if (is.null(relation)) {
    cli::cli_abort(c(
      "{.arg relation} is required for {.val finlex} input.",
      "i" = "Use {.val amends} or {.val repeals}."
    ))
  }
  relation <- rlang::arg_match(relation, c("amends", "repeals"))
  require_cols(x, c("source_year", "source_number",
                    "target_year", "target_number"), "finlex")
  keep <- !is.na(x$target_year) & !is.na(x$target_number)
  x <- x[keep, , drop = FALSE]
  n <- nrow(x)
  lex_citations(
    from_id = lex_id(rep("FI", n),
                     paste(x$source_year, x$source_number, sep = "/")),
    to_id = lex_id(rep("FI", n),
                   paste(x$target_year, x$target_number, sep = "/")),
    relation = rep(relation, n),
    source = rep("finlex", n)
  )
}

cites_from_eurlex <- function(x) {
  require_cols(x, c("celex", "citationcelex"), "eurlex")
  parts <- strsplit(as.character(x$citationcelex), ",\\s*")
  from <- rep(as.character(x$celex), lengths(parts))
  to <- trimws(unlist(parts))
  keep <- !is.na(from) & !is.na(to) & nzchar(to)
  n <- sum(keep)
  lex_citations(
    from_id = lex_id(rep("EU", n), from[keep]),
    to_id = lex_id(rep("EU", n), to[keep]),
    relation = rep("cites", n),
    source = rep("eurlex", n)
  )
}
