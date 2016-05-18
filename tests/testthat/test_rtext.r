#### rtext ==============================================================================

context("rtext init")
test_that("rtext initialization", {
  expect_error( rtext$new(), NA)
  expect_error( rtext$new(NULL), NA)
  expect_error( rtext$new(""), NA)
})



context("rtext add")
test_that("rtext add", {
  expect_true( rtext$new(text="----")$char_add("///"   )$text_get()=="----///" )
  expect_true( rtext$new(text="----")$char_add("///", 0)$text_get()=="///----" )
  expect_true( rtext$new(text="----")$char_add("///", 2)$text_get()=="--///--" )
  expect_true( rtext$new(text="----")$char_add("/")$char_add("/")$text_get()=="----//" )
})



context("rtext delete")
test_that("rtext delete", {
 expect_true( rtext$new(text="12345")$char_delete(from= 1)$text_get()=="")
 expect_true( rtext$new(text="12345")$char_delete(from=-2)$text_get()=="")
 expect_true( rtext$new(text="12345")$char_delete(from= 3)$text_get()=="12")
 expect_true( rtext$new(text="12345")$char_delete(from= 9)$text_get()=="12345")

 expect_true( rtext$new(text="12345")$char_delete(to= 1)$text_get()=="2345")
 expect_true( rtext$new(text="12345")$char_delete(to= 3)$text_get()=="45")
 expect_true( rtext$new(text="12345")$char_delete(to= 9)$text_get()=="")
 expect_true( rtext$new(text="12345")$char_delete(to=-9)$text_get()=="12345")

 expect_true( rtext$new(text="12345")$char_delete(n = 1, from = 1)$text_get()=="2345")
 expect_true( rtext$new(text="12345")$char_delete(n = 4, from = 3)$text_get()=="12")
 expect_true( rtext$new(text="12345")$char_delete(n = 0, from = 1)$text_get()=="12345")
 expect_true( rtext$new(text="12345")$char_delete(n = 5, from = -2)$text_get()=="345")

 expect_true( rtext$new(text="12345")$char_delete(n = 0, to   = 1)$text_get()=="12345")
 expect_true( rtext$new(text="12345")$char_delete(n = 1, to   = 1)$text_get()=="2345")
 expect_true( rtext$new(text="12345")$char_delete(n = 9, to   = 1)$text_get()=="2345")
 expect_true( rtext$new(text="12345")$char_delete(n = 1, to   = 9)$text_get()=="12345")
 expect_true( rtext$new(text="12345")$char_delete(n = 2, to   = 6)$text_get()=="1234")

 expect_true( rtext$new(text="12345")$char_delete(from = 2, to = 3)$text_get()=="145")
 expect_true( rtext$new(text="12345")$char_delete(from = -2, to = 3)$text_get()=="45")
 expect_true( rtext$new(text="12345")$char_delete(from =  2, to = 30)$text_get()=="1")
 expect_true( rtext$new(text="12345")$char_delete(from =  1, to = 5)$text_get()=="")
 expect_true( rtext$new(text="12345")$char_delete(from =  4, to = 4)$text_get()=="1235")
 expect_true( rtext$new(text="12345")$char_delete(from =  5, to = 4)$text_get()=="12345")
})


context("rtext hash_text")
test_that("rtext hash_text works", {
  expect_true({
    dings <- rtext$new(paste0(sample(letters, 100, replace = TRUE), collapse = ""))
    hash1 <- dings$text_hash()
    hash2 <- dings$text_hash()
    hash1 == hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$text_hash()
    dings$char_add("/")
    hash2 <- dings$text_hash()
    hash1 != hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$text_hash()
    dings$char_add("")
    hash2 <- dings$text_hash()
    hash1 == hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$text_hash()
    dings$char_delete()
    hash2 <- dings$text_hash()
    hash1 == hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$text_hash()
    dings$char_delete(1)
    hash2 <- dings$text_hash()
    hash1 != hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$text_hash()
    dings$char_delete(from = 1)
    hash2 <- dings$text_hash()
    hash1 != hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$text_hash()
    dings$char_delete(to = 1)
    hash2 <- dings$text_hash()
    hash1 != hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$text_hash()
    dings$char_delete(from = 1, to = 1)
    hash2 <- dings$text_hash()
    hash1 != hash2
  })
})



context("rtext text_get")
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
})


