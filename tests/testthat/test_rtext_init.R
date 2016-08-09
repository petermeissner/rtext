
context("rtext init") # ========================================================
test_that("rtext initialization", {
  expect_error( rtext$new(), NA)
  expect_error( rtext$new(NULL), NA)
  expect_error( rtext$new(""), NA)
  expect_error( rtext$new(text_file=test_file("rc_1_ch1.txt")), NA)
  expect_error( rtext$new(text="", text_file=test_file("rc_1_ch1.txt")), NA)
  expect_error( rtext$new(text=readLines(test_file("rc_1_ch1.txt"))), NA)
  expect_true({
    !is.null(rtext$new("")$id)
  })
  expect_true({
    identical(rtext$new("", id="mänämüdderschnüdetspück")$id, "mänämüdderschnüdetspück")
  })
  expect_true({
    all(
      nchar(
        rtext$new(text_file=test_file("test_init1.txt"))$char_get()
      )==1
    )
  })
})
