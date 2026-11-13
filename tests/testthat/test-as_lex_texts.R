test_that("finlex text is split into number, heading and body", {
  x <- as_lex_texts(fixture_finlex_text(), "finlex")
  expect_s3_class(x, "lex_texts")
  expect_equal(x$lex_id, c("FI:2025/100", "FI:2025/100"))
  expect_equal(x$section, c("1", "3 a"))
  expect_equal(x$section_native, c("1 \u00a7", "3 a \u00a7"))
  expect_equal(x$heading, c("Soveltamisala", "Voimaantulo"))
  expect_equal(x$text[1], "T\u00e4m\u00e4 asetus koskee suoritteita.")
  expect_true(all(is.na(x$subsection)))
  expect_equal(x$language, c("fi", "fi"))
})

test_that("danlex provisions require documents to resolve identifiers", {
  expect_error(
    as_lex_texts(fixture_danlex_paragraphs(), "danlex"),
    "required"
  )
  x <- as_lex_texts(fixture_danlex_paragraphs(), "danlex",
                    documents = fixture_danlex_docs_for_texts())
  expect_equal(unique(x$lex_id), "DK:B20250005005")
  expect_equal(x$provision_index, 1:3)
  expect_equal(x$section, c("1", "1", "2"))
  expect_equal(x$subsection, c("1", "2", "1"))
  expect_false(any(x$is_amendment))
})

test_that("danlex provisions without a matching document are rejected", {
  docs <- lex_documents(jurisdiction = "DK", doc_id = "X",
                        eli = "https://retsinformation.dk/eli/lta/1999/1")
  expect_error(
    as_lex_texts(fixture_danlex_paragraphs(), "danlex", documents = docs),
    "No entry"
  )
})

test_that("eurlex text becomes one row per document", {
  expect_error(as_lex_texts(fixture_eurlex_text(), "eurlex"), "required")
  x <- as_lex_texts(fixture_eurlex_text(), "eurlex", id = "32010L0013")
  expect_equal(nrow(x), 1)
  expect_equal(x$lex_id, "EU:32010L0013")
  expect_true(is.na(x$section))
  expect_null(names(x$text))
})

test_that("arguments for other sources are rejected", {
  expect_error(
    as_lex_texts(fixture_finlex_text(), "finlex", id = "1"),
    "eurlex"
  )
})
