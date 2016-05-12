#### dp_text ==============================================================================
context("dp_text")

test_that("dp_text initialization", {
  expect_true( length(dp_text$new("Hallo World")$text)==1 )
  expect_true( length(dp_text$new(c("Hallo","World"))$text)==1 )
  expect_true( length(dp_text$new()$text)==1 )
  expect_error( dp_text$new(), NA)
  expect_error( dp_text$new(NULL), NA)
  expect_error( dp_text$new(""), NA)
  expect_error( dp_text$new("", tokenize="words"), NA)
  expect_error( dp_text$new("", tokenize="lines"), NA)
  expect_error( dp_text$new("", tokenize="paragraphs"), NA)
  expect_true(
    dim(
      dp_text$new(
        "mein papa schneidet super speck",
        tokenize="words"
      )$token
    )[1]==9
  )
  expect_true(
    dim(
      dp_text$new(
        "mein\npapa\nschneidet\nsuper\nspeck",
        tokenize="lines"
      )$token
    )[1]==9
  )
  expect_true(
    dim(
      dp_text$new(
        "mein\n\npapa\n\nschneidet\n\nsuper\n\nspeck",
        tokenize="paragraphs"
      )$token
    )[1]==9
  )
})
