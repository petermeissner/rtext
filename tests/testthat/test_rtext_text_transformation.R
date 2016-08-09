
context("rtext char_add") # ====================================================
test_that("rtext add", {
  expect_true( rtext$new(text="----")$char_add("///"   )$text_get()=="----///" )
  expect_true( rtext$new(text="----")$char_add("///", 0)$text_get()=="///----" )
  expect_true( rtext$new(text="----")$char_add("///", 2)$text_get()=="--///--" )
  expect_true( rtext$new(text="----")$char_add("/")$char_add("/")$text_get()=="----//" )
  expect_true( rtext$new(text="----")$char_add("ä", Inf)$text_get()=="----ä" )
})


context("rtext char_replace") # ================================================
test_that("rtext add", {
  expect_true( rtext$new(text="12345")$char_replace(1,1,"///" )$text_get()=="///2345" )
  expect_true( rtext$new(text="12345")$char_replace(5,5,"///" )$text_get()=="1234///" )
  expect_true( rtext$new(text="12345")$char_replace(3,3,"///" )$text_get()=="12///45" )
  expect_true( rtext$new(text="12345")$char_replace(1,5,"///" )$text_get()=="///"     )
  expect_true( rtext$new(text="12345")$char_replace(0,9,"///" )$text_get()=="///"     )
  expect_true( rtext$new(text="12345")$char_replace(0,0,"///" )$text_get()=="///12345")
  expect_true( rtext$new(text="12345")$char_replace(0,0,"ä" )$text_get()=="ä12345")
})



context("rtext char_delete") # =================================================
test_that("rtext char_delete", {
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

  expect_true(
    rtext$new(text="12345")$char_delete(n = 0, to   = 5)$text_get()==
      rtext$new(text="12345")$char_delete(0)$text_get()
  )
  expect_true(
    rtext$new(text="12345")$char_delete(n = 3, to   = 5)$text_get()==
      rtext$new(text="12345")$char_delete(3)$text_get()
  )
})
