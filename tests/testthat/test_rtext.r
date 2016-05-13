#### rtext ==============================================================================
context("rtext")

test_that("rtext initialization", {
  expect_true( length(rtext$new("Hallo World")$text)==1 )
  expect_true( length(rtext$new(c("Hallo","World"))$text)==1 )
  expect_true( length(rtext$new()$text)==1 )
  expect_error( rtext$new(), NA)
  expect_error( rtext$new(NULL), NA)
  expect_error( rtext$new(""), NA)
  expect_error( rtext$new("", tokenizer="paragraphs"))
  expect_true(
    dim(
      rtext$new(
        "mein papa schneidet super speck",
        tokenize_by="\\W"
      )$token
    )[1]==9
  )
  expect_true(
    dim(
      rtext$new(
        "mein\npapa\nschneidet\nsuper\nspeck",
        tokenize_by="\n"
      )$token
    )[1]==9
  )
  expect_true(
    dim(
      rtext$new(
        "mein\n\npapa\n\nschneidet\n\nsuper\n\nspeck",
        tokenize_by="\\s*\n\\s*\n\\s*"
      )$token
    )[1]==9
  )
  expect_true({
      all(dim( rtext$new("Hollah die Waldfee.")$token ) > 0)
  })
  expect_true({
    text <- rtext$new("meine mudder schneidet speck", tokenize_by="")
    text <- rtext$new("meine mudder schneidet speck")
    TRUE
  })
})
