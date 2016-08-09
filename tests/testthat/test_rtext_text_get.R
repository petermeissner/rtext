context("rtext text_get") # ====================================================
test_that("get_text works also with junk input", {
  expect_true( # general
    all(
      rtext$new(text="1234567890")$text_get(length = 1          )=="1",
      rtext$new(text="1234567890")$text_get(length = 1, to   = 1)=="1",
      rtext$new(text="1234567890")$text_get(length = 1, from = 1)=="1"
    )
  )
  expect_true( # length
    all(
      rtext$new(text="1234567890")$text_get(length =  0)=="",
      rtext$new(text="1234567890")$text_get(length = -1)=="",
      rtext$new(text="1234567890")$text_get(length = 11)=="1234567890"
    )
  )
  expect_true( # length + from
    all(
      rtext$new(text="1234567890")$text_get(from =   0, length =  0)=="",
      rtext$new(text="1234567890")$text_get(from =  10, length = -1)=="",
      rtext$new(text="1234567890")$text_get(from = -10, length = 10)=="",
      rtext$new(text="1234567890")$text_get(from =  -1, length = 3)=="1",
      rtext$new(text="1234567890")$text_get(from =  11, length =  1)=="",
      rtext$new(text="1234567890")$text_get(from =   1, length =  3)=="123"
    )

  )
  expect_true( # length + to
    all(
      rtext$new(text="1234567890")$text_get(length =  1, to = 11)=="",
      rtext$new(text="1234567890")$text_get(length =  1, to = 10)=="0",
      rtext$new(text="1234567890")$text_get(length =  1, to = -1)=="",
      rtext$new(text="1234567890")$text_get(length = -1, to = -1)=="",
      rtext$new(text="1234567890")$text_get(length = -1, to =  3)=="",
      rtext$new(text="1234567890")$text_get(length =  3, to =  3)=="123"
    )
  )
  expect_true( # from + to
    all(
      rtext$new(text="1234567890")$text_get(from = 2, to =  2)=="2",
      rtext$new(text="1234567890")$text_get(from = 2, to =  1)=="21",
      rtext$new(text="1234567890")$text_get(from = 0, to =  2)=="12",
      rtext$new(text="1234567890")$text_get(from = 9, to = 22)=="90"
    )
  )
  expect_true( # from + to
    all(
      rtext$new(text="1234567890")$text_get(from = 9, to = 22, split="")==c("9","0"),
      rtext$new(text="1\n2")$text_get(split="\n")==c("1","2")
    )
  )
  expect_true(
    rtext$new(text="12345")$char_delete(n = 1)$text_get() != "NANANANA"
  )
})


