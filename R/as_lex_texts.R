#' Convert text output of data packages to the lexverse standard form
#'
#' Turns the provision-level text output of a lexverse data package into a
#' [lex_texts()] table. Like [as_lex_documents()], the converters do not
#' call the source packages.
#'
#' Supported inputs:
#' * `"finlex"`: output of `flx_get_text()`. The section number and heading
#'   that the source prefixes to each text are removed from `text`, and the
#'   heading is kept in its own column. Subsections are not separated by
#'   the source.
#' * `"danlex"`: output of `dlx_get_paragraphs()`. The source identifies
#'   documents by ELI only, so `documents` must be a [lex_documents()] table
#'   covering them, from which the accession-number identifiers are taken.
#' * `"eurlex"`: output of `elx_fetch_data(type = "text")`, a character
#'   vector of whole-document texts. The CELEX numbers must be supplied in
#'   `id`, and each text becomes one row with `section` missing.
#'
#' @param x Output of a lexverse data package.
#' @param source Name of the package that produced `x`.
#' @param documents For `source = "danlex"` only: a [lex_documents()] table
#'   with the `eli` of every document in `x`.
#' @param id For `source = "eurlex"` only: CELEX numbers, one per text.
#' @param language Language of the text. Defaults to `"fi"` for finlex,
#'   `"da"` for danlex and `"en"` for eurlex.
#'
#' @return A [lex_texts()] tibble.
#' @export
#' @examples
#' finlex_output <- data.frame(
#'   year = 2025L, number = 100L, status = "ok",
#'   section_index = 1L, section_id = "sec_1",
#'   heading = "Soveltamisala",
#'   text = "1 \u00a7SoveltamisalaT\u00e4m\u00e4 asetus koskee ..."
#' )
#' as_lex_texts(finlex_output, source = "finlex")
as_lex_texts <- function(x, source = c("danlex", "finlex", "eurlex"),
                         documents = NULL, id = NULL, language = NULL) {
  source <- rlang::arg_match(source)
  if (!is.null(documents) && source != "danlex") {
    cli::cli_abort("{.arg documents} applies to {.val danlex} input only.")
  }
  if (!is.null(id) && source != "eurlex") {
    cli::cli_abort("{.arg id} applies to {.val eurlex} input only.")
  }
  if (is.null(language)) {
    language <- switch(source, danlex = "da", finlex = "fi", eurlex = "en")
  }
  switch(source,
    danlex = texts_from_danlex(x, documents, language),
    finlex = texts_from_finlex(x, language),
    eurlex = texts_from_eurlex(x, id, language)
  )
}

texts_from_finlex <- function(x, language) {
  if (!is.data.frame(x)) cli::cli_abort("{.arg x} must be a data frame.")
  require_cols(x, c("year", "number", "section_index", "section_id",
                    "heading", "text"), "finlex")
  x <- x[!is.na(x$text), , drop = FALSE]
  n <- nrow(x)
  text <- as.character(x$text)
  heading <- as.character(x$heading)

  prefix <- "^\\s*(\\d+\\s*[[:alpha:]]?)\\s*\u00a7"
  has_prefix <- grepl(prefix, text, perl = TRUE)
  captured <- trimws(sub(paste0(prefix, "(?s).*$"), "\\1", text, perl = TRUE))
  native <- ifelse(has_prefix, paste(captured, "\u00a7"), NA_character_)
  number <- ifelse(has_prefix, captured, sub("^sec_", "", x$section_id))

  body <- sub(prefix, "", text, perl = TRUE)
  body <- sub("^\\s+", "", body)
  strip <- !is.na(heading) & nzchar(heading) & startsWith(body, heading)
  body[strip] <- substring(body[strip], nchar(heading[strip]) + 1)

  lex_texts(
    lex_id = lex_id(rep("FI", n), paste(x$year, x$number, sep = "/")),
    language = rep(language, n),
    provision_index = as.integer(x$section_index),
    section = normalise_section(number),
    section_native = native,
    heading = heading,
    text = trimws(body),
    source = rep("finlex", n)
  )
}

texts_from_danlex <- function(x, documents, language) {
  if (!is.data.frame(x)) cli::cli_abort("{.arg x} must be a data frame.")
  require_cols(x, c("eli", "paragraf", "stk_index", "is_amendment", "text"),
               "danlex")
  if (is.null(documents)) {
    cli::cli_abort(c(
      "{.arg documents} is required for {.val danlex} input.",
      "i" = "Supply {.code as_lex_documents()} output for the same documents, so that ELIs can be matched to accession numbers."
    ))
  }
  if (!is_lex_documents(documents)) {
    cli::cli_abort("{.arg documents} must be a {.cls lex_documents} table.")
  }
  ids <- documents$lex_id[match(x$eli, documents$eli)]
  unmatched <- unique(x$eli[is.na(ids)])
  if (length(unmatched) > 0) {
    cli::cli_abort(
      "No entry in {.arg documents} for ELI{?s} {.val {unmatched}}."
    )
  }
  n <- nrow(x)
  provision_index <- integer(n)
  for (u in unique(ids)) {
    w <- ids == u
    provision_index[w] <- seq_len(sum(w))
  }
  lex_texts(
    lex_id = ids,
    language = rep(language, n),
    provision_index = provision_index,
    section = normalise_section(x$paragraf),
    subsection = as.character(x$stk_index),
    section_native = as.character(x$paragraf),
    is_amendment = as.logical(x$is_amendment),
    text = as.character(x$text),
    source = rep("danlex", n)
  )
}

texts_from_eurlex <- function(x, id, language) {
  if (!is.character(x)) {
    cli::cli_abort("{.arg x} must be a character vector of texts.")
  }
  if (is.null(id)) {
    cli::cli_abort(c(
      "{.arg id} is required for {.val eurlex} input.",
      "i" = "Supply the CELEX number of each text."
    ))
  }
  if (length(id) != length(x)) {
    cli::cli_abort("{.arg id} must have one CELEX number per text.")
  }
  n <- length(x)
  lex_texts(
    lex_id = lex_id(rep("EU", n), id),
    language = rep(language, n),
    provision_index = rep(1L, n),
    text = unname(x),
    source = rep("eurlex", n)
  )
}
