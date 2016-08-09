context("rtext char_data_get()") # =================================================================

test_that("rtext char_data_get() works", {
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_get()
    TRUE
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("data",1,1)
    all(names(dings$char_data_get()) == c("char", "i", "data"))
  })
})

context("rtext char_data_set()") # ========================================================

test_that("rtext char_data_set() works", {
  expect_error({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 1, 1:4)
  })
  expect_error({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 1:4, 1)
  })
  expect_error({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 0, 1)
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 1, 1)
    dings$char_data_set("var2", 3, 1)
    res <- dings$char_data_get()
    all(
      dim(res)==c(2,4),
      c("var1", "var2") %in% names(res)
    )
  })
  expect_true({
    dings <- rtext$new("abcdefghijklmnopqrstuvw")
    dings$char_data_set("pimpf", c(1,2,6,7) , 1)
    dings$char_data_set("pompf", c(1:4,7:10), 1)
    all(
      dings$char_data_get()==
        structure(list(char = c("a", "b", "c", "d", "f", "g", "h", "i",  "j"), i = c(1, 2, 3, 4, 6, 7, 8, 9, 10), pimpf = c(1, 1, NA,  NA, 1, 1, NA, NA, NA), pompf = c(1, 1, 1, 1, NA, 1, 1, 1, 1)), .Names = c("char",  "i", "pimpf", "pompf"), row.names = c(NA, 9L), class = "data.frame"),
      na.rm = TRUE
    )
  })
})

test_that("rtext code updates on char_delete", {
  expect_true({
    dings <- rtext$new("123")
    dings$char_get()
    dings$char_data_set("var1", 1, 1)
    dings$char_data_get()
    dings$char_delete(1, from = 1)
    identical(
      dings$char_data_get(),
      data.frame()
    )
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 3, 1)

    dings$char_data_get()
    dings$char_get()

    dings$char_delete(1, from = 1)
    identical(
      unlist(dings$char_data_get()),
      unlist(data.frame(char="3", i=2, var1=1))
    )
  })
})

test_that("rtext code updates on char_add", {
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 1, 1)
    dings$char_add("a",0)
    dings$char_get()
    identical(
      dings$char_data_get(),
      data.frame(char="1", i=2, var1=1)
    )
  })
})

test_that("rtext code updates on char_replace", {
  expect_true({
    dings <- rtext$new("abcdefg")
    dings$char_data_set("var1", 3, 1)
    dings$char_replace(from=2,to=6,by="/")
    dings$char_get()
    identical(
      dings$char_data_get(),
      data.frame()
    )
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 1, 1)
    dings$char_replace(1,1,"a")
    dings$char_get()
    identical(
      dings$char_data_get(),
      data.frame()
    )
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 3, 1)
    dings$char_replace(1,2,"")
    dings$char_get()
    dings$char_data_get()
    identical(
      unlist(dings$char_data_get(), use.names = FALSE),
      c("3",1,1)
    )
  })
})


