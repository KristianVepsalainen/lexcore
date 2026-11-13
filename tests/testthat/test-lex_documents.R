test_that("lex_documents fills absent columns with typed missing values", {
  x <- lex_documents(
    jurisdiction = "FI", doc_id = "2025/100",
    date_document = as.Date("2025-03-18"), status = "ok", source = "finlex"
  )
  expect_s3_class(x, "lex_documents")
  expect_s3_class(x, "tbl_df")
  expect_equal(x$lex_id, "FI:2025/100")
  expect_type(x$doc_type, "character")
  expect_true(is.na(x$doc_type))
  expect_s3_class(x$date_in_force, "Date")
  expect_type(x$in_force, "logical")
})

test_that("extra columns follow the standard ones", {
  x <- lex_documents(jurisdiction = "DK", doc_id = "B19980076305",
                     ministry = "Arbejdsministeriet")
  expect_equal(names(x)[1], "lex_id")
  expect_equal(names(x)[ncol(x)], "ministry")
})

test_that("a zero-row table is valid", {
  x <- lex_documents()
  expect_equal(nrow(x), 0)
  expect_true(is_lex_documents(x))
})

test_that("wrong column types are rejected", {
  expect_error(
    lex_documents(jurisdiction = "FI", doc_id = "1",
                  date_document = "2025-03-18"),
    "must be"
  )
})

test_that("missing and duplicated identifiers are rejected", {
  expect_error(lex_documents(doc_id = "1"), "missing values")
  expect_error(
    lex_documents(jurisdiction = c("FI", "FI"), doc_id = c("1", "1")),
    "unique"
  )
})

test_that("unknown status values are rejected", {
  expect_error(
    lex_documents(jurisdiction = "FI", doc_id = "1", status = "fine"),
    "Unknown"
  )
})
