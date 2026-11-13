test_that("documents from several sources bind into one table", {
  fi <- as_lex_documents(fixture_finlex_meta(), "finlex")
  eu <- as_lex_documents(fixture_eurlex(), "eurlex")
  x <- lex_bind(fi, eu)
  expect_s3_class(x, "lex_documents")
  expect_equal(nrow(x), 3)
  expect_setequal(x$jurisdiction, c("FI", "EU"))
})

test_that("exact duplicates are kept once and NULL is ignored", {
  fi <- as_lex_documents(fixture_finlex_meta(), "finlex")
  x <- lex_bind(fi, NULL, fi)
  expect_equal(nrow(x), 1)
})

test_that("conflicting rows for the same document are an error", {
  a <- lex_documents(jurisdiction = "FI", doc_id = "1", title = "A")
  b <- lex_documents(jurisdiction = "FI", doc_id = "1", title = "B")
  expect_error(lex_bind(a, b), "unique")
})

test_that("extra columns are kept and filled with missing values", {
  a <- lex_documents(jurisdiction = "DK", doc_id = "1", ministry = "X")
  b <- lex_documents(jurisdiction = "FI", doc_id = "2")
  x <- lex_bind(a, b)
  expect_equal(x$ministry, c("X", NA))
})

test_that("different classes cannot be mixed", {
  d <- lex_documents(jurisdiction = "FI", doc_id = "1")
  cit <- lex_citations(from_id = "FI:1", to_id = "FI:2", relation = "cites")
  expect_error(lex_bind(d, cit), "same class")
  expect_error(lex_bind(data.frame(a = 1)), "same class")
  expect_error(lex_bind(), "at least one")
})

test_that("duplicated relations are kept once", {
  a <- lex_citations(from_id = "FI:1", to_id = "FI:2", relation = "cites",
                     source = "x")
  b <- lex_citations(from_id = "FI:1", to_id = "FI:2", relation = "cites",
                     source = "y")
  expect_equal(nrow(lex_bind(a, b)), 1)
})

test_that("texts bind across sources", {
  fi <- as_lex_texts(fixture_finlex_text(), "finlex")
  eu <- as_lex_texts(fixture_eurlex_text(), "eurlex", id = "32010L0013")
  x <- lex_bind(fi, eu)
  expect_s3_class(x, "lex_texts")
  expect_equal(nrow(x), 3)
})
