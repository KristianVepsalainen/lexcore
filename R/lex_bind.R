#' Combine lexcore tables from several sources
#'
#' Binds `lex_documents`, `lex_citations` or `lex_texts` tables row-wise,
#' typically the converted output of several data packages, so that
#' legislation from different jurisdictions can be analysed together.
#'
#' All inputs must be of the same class. Extra columns present in only some
#' inputs are kept and filled with missing values elsewhere. Rows that are
#' exact duplicates are kept once, as are duplicated relations in
#' `lex_citations`. Two different rows for the same document or provision
#' are an error, since the result would be ambiguous.
#'
#' @param ... Tables of one lexcore class. `NULL` inputs are ignored.
#'
#' @return A table of the same class as the inputs.
#' @export
#' @examples
#' fi <- lex_documents(jurisdiction = "FI", doc_id = "2025/100",
#'                     source = "finlex")
#' eu <- lex_documents(jurisdiction = "EU", doc_id = "32010L0013",
#'                     doc_type = "directive", source = "eurlex")
#' lex_bind(fi, eu)
lex_bind <- function(...) {
  xs <- Filter(Negate(is.null), list(...))
  if (length(xs) == 0) {
    cli::cli_abort("Supply at least one lexcore table.")
  }
  classes <- vapply(xs, lex_class, character(1))
  if (anyNA(classes) || length(unique(classes)) > 1) {
    cli::cli_abort(c(
      "All inputs must be lexcore tables of the same class.",
      "i" = "Use {.fn lex_documents}, {.fn lex_citations} or {.fn lex_texts} tables, one class per call."
    ))
  }
  cls <- classes[[1]]

  proto <- lex_prototype(cls)
  for (x in xs) {
    for (nm in setdiff(names(x), names(proto))) {
      proto[[nm]] <- x[[nm]][0]
    }
  }
  parts <- lapply(xs, function(x) as.data.frame(fill_columns(x, proto)))
  out <- tibble::as_tibble(do.call(rbind, parts))
  out <- out[!duplicated(out), , drop = FALSE]
  if (cls == "lex_citations") {
    out <- out[!duplicated(out[c("from_id", "to_id", "relation")]), ,
               drop = FALSE]
  }

  out <- tibble::new_tibble(out, class = cls)
  validator <- switch(cls,
    lex_documents = validate_lex_documents,
    lex_citations = validate_lex_citations,
    lex_texts = validate_lex_texts
  )
  validator(out)
  out
}

lex_class <- function(x) {
  for (cls in c("lex_documents", "lex_citations", "lex_texts")) {
    if (inherits(x, cls)) return(cls)
  }
  NA_character_
}

lex_prototype <- function(cls) {
  switch(cls,
    lex_documents = lex_documents_prototype(),
    lex_citations = lex_citations_prototype(),
    lex_texts = lex_texts_prototype()
  )
}
