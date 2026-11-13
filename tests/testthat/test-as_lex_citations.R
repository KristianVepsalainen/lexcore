test_that("danlex references become based_on relations", {
  x <- as_lex_citations(fixture_danlex_refs(), "danlex")
  expect_equal(nrow(x), 2)
  expect_equal(unique(x$relation), "based_on")
  expect_equal(x$to_id[1], "DK:A19990059229")
  expect_s3_class(x$to_version_date, "Date")
})

test_that("finlex relations require and use the relation argument", {
  expect_error(as_lex_citations(fixture_finlex_affected(), "finlex"),
               "required")
  x <- as_lex_citations(fixture_finlex_affected(), "finlex",
                        relation = "repeals")
  expect_equal(nrow(x), 1)
  expect_equal(x$from_id, "FI:2023/2")
  expect_equal(x$to_id, "FI:2015/1352")
  expect_equal(x$relation, "repeals")
})

test_that("eurlex citations are split, cleaned and deduplicated", {
  x <- as_lex_citations(fixture_eurlex(), "eurlex")
  expect_equal(nrow(x), 2)
  expect_equal(unique(x$relation), "cites")

  joined <- data.frame(celex = "32010L0013",
                       citationcelex = "32003R2152, 52004IP0373")
  expect_equal(nrow(as_lex_citations(joined, "eurlex")), 2)
})

test_that("relation is rejected for sources other than finlex", {
  expect_error(
    as_lex_citations(fixture_danlex_refs(), "danlex", relation = "amends"),
    "finlex"
  )
})
