test_that("lex_citations builds a valid edge table", {
  x <- lex_citations(
    from_id = c("DK:B19980076305", "DK:B19980076305"),
    to_id = c("DK:A19990059229", "DK:A19990066629"),
    relation = "based_on",
    to_version_date = as.Date(c("1999-07-14", "1999-08-19")),
    source = "danlex"
  )
  expect_s3_class(x, "lex_citations")
  expect_equal(nrow(x), 2)
})

test_that("cross-jurisdiction relations are allowed", {
  x <- lex_citations(from_id = "FI:2023/2", to_id = "EU:32010L0013",
                     relation = "implements")
  expect_true(is_lex_citations(x))
  expect_true(is.na(x$to_version_date))
})

test_that("unknown relations and malformed ids are rejected", {
  expect_error(
    lex_citations(from_id = "FI:1", to_id = "FI:2", relation = "likes"),
    "Unknown"
  )
  expect_error(
    lex_citations(from_id = "2023/2", to_id = "FI:2", relation = "cites"),
    "Not a valid"
  )
})
