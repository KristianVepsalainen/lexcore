test_that("danlex output converts, dropping unidentified rows", {
  expect_warning(
    x <- as_lex_documents(fixture_danlex_doc(), "danlex"),
    "Dropped 1 row"
  )
  expect_equal(nrow(x), 1)
  expect_equal(x$lex_id, "DK:B19980076305")
  expect_equal(x$doc_type, "regulation")
  expect_false(x$in_force)
  expect_equal(x$language, "da")
})

test_that("finlex output converts with missing type and force status", {
  x <- as_lex_documents(fixture_finlex_meta(), "finlex")
  expect_equal(x$lex_id, "FI:2025/100")
  expect_true(is.na(x$doc_type))
  expect_true(is.na(x$in_force))
})

test_that("eurlex output is deduplicated and coerced", {
  x <- as_lex_documents(fixture_eurlex(), "eurlex")
  expect_equal(nrow(x), 2)
  expect_equal(x$doc_type, c("directive", "directive"))
  expect_true(all(x$in_force))
  expect_s3_class(x$date_document, "Date")
  expect_match(x$source_url[1], "CELEX:32010L0013")
})

test_that("input from the wrong package is rejected", {
  expect_error(
    as_lex_documents(fixture_finlex_meta(), "danlex"),
    "does not look like"
  )
})
