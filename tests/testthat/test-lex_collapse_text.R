test_that("provisions are joined in document order", {
  x <- lex_texts(
    lex_id = "FI:1", language = "fi",
    provision_index = c(2L, 1L), text = c("toinen", "eka")
  )
  out <- lex_collapse_text(x, sep = " | ")
  expect_s3_class(out, "lex_texts")
  expect_equal(nrow(out), 1)
  expect_equal(out$text, "eka | toinen")
  expect_true(is.na(out$section))
})

test_that("documents and languages are collapsed separately", {
  x <- lex_texts(
    lex_id = c("FI:1", "FI:1", "FI:2"),
    language = c("fi", "sv", "fi"),
    provision_index = c(1L, 1L, 1L),
    text = c("a", "b", "c")
  )
  expect_equal(nrow(lex_collapse_text(x)), 3)
})

test_that("amending provisions can be dropped", {
  x <- lex_texts(
    lex_id = "DK:1", language = "da", provision_index = 1:2,
    is_amendment = c(FALSE, TRUE), text = c("egen", "ny tekst")
  )
  expect_equal(lex_collapse_text(x, drop_amendments = TRUE)$text, "egen")
  expect_equal(lex_collapse_text(x, sep = " ")$text, "egen ny tekst")
})

test_that("non-lex_texts input is rejected", {
  expect_error(lex_collapse_text(data.frame(text = "a")), "lex_texts")
})
