#### dp_text ==============================================================================
context("dp_text")

test_that("dp_text initialization", {
  expect_true( length(dp_text$new("Hallo World")$text)==1 )
  expect_true( length(dp_text$new(c("Hallo","World"))$text)==1 )
  expect_true( length(dp_text$new()$text)==1 )

})
