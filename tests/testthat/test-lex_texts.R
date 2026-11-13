test_that("lex_texts fills absent columns with typed missing values", {
  x <- lex_texts(lex_id = "FI:2025/100", language = "fi",
                 provision_index = 1L, text = "Teksti", source = "finlex")
  expect_s3_class(x, "lex_texts")
  expect_true(is.na(x$section))
  expect_type(x$is_amendment, "logical")
})

test_that("duplicated provisions are rejected", {
  expect_error(
    lex_texts(lex_id = "FI:2025/100", language = "fi",
              provision_index = c(1L, 1L), text = c("a", "b")),
    "once"
  )
})

test_that("the same provision may exist in several languages", {
  x <- lex_texts(lex_id = "FI:2025/100", language = c("fi", "sv"),
                 provision_index = c(1L, 1L), text = c("a", "b"))
  expect_equal(nrow(x), 2)
})

test_that("provision_index must be integer and present", {
  expect_error(
    lex_texts(lex_id = "FI:1", provision_index = 1, text = "a"),
    "must be"
  )
  expect_error(
    lex_texts(lex_id = "FI:1", provision_index = NA_integer_, text = "a"),
    "missing"
  )
})

test_that("normalise_section unifies national conventions", {
  expect_equal(
    normalise_section(c("1 \u00a7", "\u00a7 1.", "Article 12", "3a \u00a7",
                        "3 a \u00a7", "", NA)),
    c("1", "1", "12", "3 a", "3 a", NA, NA)
  )
})
