test_that("lex_id joins jurisdiction and id", {
  expect_equal(lex_id("FI", "2025/100"), "FI:2025/100")
  expect_equal(
    lex_id(c("DK", "EU"), c("B19980076305", "32010L0013")),
    c("DK:B19980076305", "EU:32010L0013")
  )
})

test_that("lex_id propagates missing values", {
  expect_true(is.na(lex_id("FI", NA)))
})

test_that("lex_id rejects malformed jurisdictions", {
  expect_error(lex_id("fi", "1"), "Invalid jurisdiction")
  expect_error(lex_id("FIN", "1"), "Invalid jurisdiction")
})

test_that("parse_lex_id splits at the first colon only", {
  p <- parse_lex_id("SE:1974:152")
  expect_equal(p$jurisdiction, "SE")
  expect_equal(p$doc_id, "1974:152")
})

test_that("lex_id and parse_lex_id round-trip", {
  ids <- c("FI:2025/100", "DK:B19980076305", "SE:1974:152", "EU:32010L0013")
  p <- parse_lex_id(ids)
  expect_equal(lex_id(p$jurisdiction, p$doc_id), ids)
})

test_that("parse_lex_id rejects malformed input", {
  expect_error(parse_lex_id("2025/100"), "Not a valid")
})
