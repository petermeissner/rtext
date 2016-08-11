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
  })
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
  })
})


context("rtext tokenize_data_regex") # =================================================================
test_that("rtext tokenize_data_regex", {
  expect_true({
    dings <- rtext$new("meine mudder schneidet speck - ho-narroh")
    dings$char_data_set("test", 1, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    all(
      dim(
        dings$tokenize_data_regex("\\W+")
      ) == c(6,7)
    )
  })
  expect_true({
    dings <- rtext$new("meine mudder schneidet speck - ho-narroh")
    dings$char_data_set("test", 1, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    all(
        is.na(dings$tokenize_data_regex("\\W+")$test)
    )
  })
  expect_true({
    dings <- rtext$new("meine mudder schneidet speck - ho-narroh")
    dings$char_data_set("test", 1, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    all(
      dings$tokenize_data_regex("\\W+")$toast=="uggah"
    )
  })
  expect_true({
    dings <- rtext$new("meine mudder schneidet speck - ho-narroh")
    dings$char_data_set("test", 1, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    !all(
      is.na(
        dings$tokenize_data_regex(
          "\\W+",
          aggregate_function = function(x){x[1]}
        )$test
      )
    )
  })
})





context("rtext tokenize_data_words") # =================================================================
test_that("rtext tokenize_data_words", {
  expect_true({
    dings <- rtext$new("meine mudder schneidet speck - ho-narroh")
    dings$char_data_set("test", 1:5, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    dings$tokenize_data_words()$test[1]==1
  })
  expect_true({
    dings <- rtext$new("meine mudder schneidet speck - ho-narroh")
    dings$char_data_set("test", 1:5, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    length(dings$tokenize_data_words()$test)==6
  })
  expect_true({
    dings <- rtext$new("meine mudder schneidet speck - ho-narroh")
    dings$char_data_set("test", 1:5, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    length(dings$tokenize_data_words(non_token = TRUE)$test)==11
  })
})

context("rtext tokenize_data_lines") # =================================================================
test_that("rtext tokenize_data_lines", {
  expect_true({
    dings <- rtext$new("meine mudder \nschneidet speck\n - ho-narroh")
    dings$char_data_set("test", 1:20, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    length(dings$tokenize_data_lines()$test) == 3
  })
  expect_true({
    dings <- rtext$new("meine mudder \nschneidet speck\n - ho-narroh")
    dings$char_data_set("test", 1:20, 1)
    dings$char_data_set("toast", 1:dings$char_length(), "uggah")
    dings$char_data_get()
    dings$tokenize_data_lines()$test[1] == 1 &
    is.na(dings$tokenize_data_lines()$test[2])
  })
})







