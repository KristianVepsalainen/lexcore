docs_for_summary <- function() {
  lex_documents(
    jurisdiction = c("FI", "FI", "DK", "EU"),
    doc_id = c("2025/1", "2024/9", "B1", "32010L0013"),
    doc_type = c(NA, NA, "regulation", "directive"),
    date_document = as.Date(c("2025-01-10", "2024-05-01",
                              "2025-03-05", "2010-03-10"))
  )
}

cites_for_summary <- function() {
  lex_citations(
    from_id = c("FI:2025/1", "FI:2025/1", "DK:B1"),
    to_id = c("FI:2024/9", "EU:32010L0013", "DK:B2"),
    relation = c("amends", "implements", "based_on")
  )
}

test_that("output is counted by jurisdiction and year", {
  out <- lex_summarise_output(docs_for_summary())
  expect_named(out, c("jurisdiction", "year", "n"))
  expect_equal(nrow(out), 4)
  expect_equal(out$n[out$jurisdiction == "FI" & out$year == 2025], 1L)
  expect_equal(sum(out$n), 4L)
})

test_that("output can be grouped by other columns", {
  out <- lex_summarise_output(docs_for_summary(), by = "jurisdiction")
  expect_equal(out$n[out$jurisdiction == "FI"], 2L)
  expect_error(lex_summarise_output(docs_for_summary(), by = "ministry"),
               "Unknown")
})

test_that("citations are summarised per document", {
  out <- lex_summarise_citations(cites_for_summary())
  fi1 <- out[out$lex_id == "FI:2025/1", ]
  expect_equal(fi1$n_out, 2L)
  expect_equal(fi1$n_out_foreign, 1L)
  expect_equal(out$n_in[out$lex_id == "EU:32010L0013"], 1L)
  expect_equal(out$n_in_foreign[out$lex_id == "EU:32010L0013"], 1L)
})

test_that("documents without relations appear when documents are given", {
  out <- lex_summarise_citations(cites_for_summary(),
                                 documents = docs_for_summary())
  expect_equal(out$lex_id, docs_for_summary()$lex_id)
  expect_false("DK:B2" %in% out$lex_id)
})

test_that("citations are summarised per jurisdiction pair", {
  out <- lex_summarise_citations(cites_for_summary(), level = "jurisdiction")
  expect_named(out, c("from_jurisdiction", "to_jurisdiction", "relation", "n"))
  expect_equal(nrow(out), 3)
  expect_equal(
    out$n[out$from_jurisdiction == "FI" & out$to_jurisdiction == "EU"], 1L
  )
  expect_error(
    lex_summarise_citations(cites_for_summary(), level = "jurisdiction",
                            documents = docs_for_summary()),
    "document"
  )
})
