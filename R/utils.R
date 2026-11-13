# Internal helpers shared by the lexcore classes

check_columns <- function(x, prototype, class_name,
                          call = rlang::caller_env()) {
  missing <- setdiff(names(prototype), names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      "A {.cls {class_name}} object is missing column{?s} {.field {missing}}.",
      call = call
    )
  }
  for (nm in names(prototype)) {
    expected <- class(prototype[[nm]])
    actual <- class(x[[nm]])
    if (!identical(expected, actual)) {
      cli::cli_abort(
        "Column {.field {nm}} must be {.cls {expected}}, not {.cls {actual}}.",
        call = call
      )
    }
  }
  invisible(x)
}

fill_columns <- function(x, prototype) {
  n <- nrow(x)
  for (nm in setdiff(names(prototype), names(x))) {
    x[[nm]] <- rep(prototype[[nm]][NA_integer_], n)
  }
  extra <- setdiff(names(x), names(prototype))
  x[c(names(prototype), extra)]
}

is_jurisdiction <- function(x) grepl("^[A-Z]{2}$", x)

is_lex_id_format <- function(x) grepl("^[A-Z]{2}:.+", x)

# Helpers for the converters -------------------------------------------------

require_cols <- function(x, cols, source, call = rlang::caller_env()) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      c("Input does not look like {.pkg {source}} output.",
        "x" = "Missing column{?s}: {.field {missing}}."),
      call = call
    )
  }
  invisible(x)
}

drop_unidentified <- function(x, unidentified) {
  n <- sum(unidentified)
  if (n > 0) {
    cli::cli_warn(
      "Dropped {n} row{?s} without an identifier, typically {.val not_found} results."
    )
  }
  x[!unidentified, , drop = FALSE]
}

doc_type_map <- list(
  danlex = c(
    "Lov" = "act",
    "Lovbekendtg\u00f8relse" = "act",
    "Bekendtg\u00f8relse" = "regulation"
  ),
  eurlex = c(
    "DIR" = "directive",
    "REG" = "regulation",
    "DEC" = "decision"
  )
)

harmonise_doc_type <- function(x, source) {
  unname(doc_type_map[[source]][as.character(x)])
}
