context("\nrtext") # =================================================================
context("rtext all methods are present") # =================================================================

test_that("rtext all methods are present", {
  expect_true({
    dings <- rtext$new("text")
    ls_list <- subset(dings$ls(), class=="function")$name
    all(
      c("hash", "hashed", "text", "char_add", "char_data_get", "char_data_set",
        "char_data_set_regex", "char_delete", "char_get", "char_length",
        "char_replace", "clone", "debug", "export", "get", "hash_get",
        "info", "initialize", "load", "ls", "message", "save", "text_get",
        "text_show", "tokenize_data_lines", "tokenize_data_regex", "tokenize_data_words"
      ) %in%
      ls_list
    )
  }, NA)
})

context("rtext all data fields are present") # =================================================================
test_that("rtext all data fields are present", {
  expect_true({
    dings <- rtext$new("text")
    ls_list <- subset(dings$ls(), !grepl("function", class) )$name
    all(
      c("char", "char_data", "hashes", "encoding", "id", "sourcetype",
        "text_file", "verbose", "save_file") %in%
      ls_list
    )
  }, NA)
})














