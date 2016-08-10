context("rtext char_data_get()") # =================================================================

test_that("rtext char_data_get() works", {
  expect_error({
    dings <- rtext$new("123")
    dings$char_data_get(0,0)
  }, NA)
  expect_true({
    dings <- rtext$new("123")
    rtext:::dim1(dings$char_data_get())==3
  })
  expect_true({
    text <- "1234567890"
    dings <- rtext$new(text)
    all(
      rtext:::dim1(dings$char_data_get(from = 20))==0,
      rtext:::dim1(dings$char_data_get(to = 0))==0,
      rtext:::dim1(dings$char_data_get(from=2, to = 1))==0,
      rtext:::dim1(dings$char_data_get(from=1, to = 1))==1,
      rtext:::dim1(dings$char_data_get(from=1, to = 1000))==nchar(text),
      rtext:::dim1(dings$char_data_get(from=-100, to = 1))==1,
      rtext:::dim1(dings$char_data_get(from=-100, to = 100))==nchar(text)
    )
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("data",1,1)
    res <- names( dings$char_data_get() )
    setequal( res, c("char", "i", "data") )
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("data1",1,1)
    dings$char_data_set("data2",1:3,1)
    res <- names( dings$char_data_get() )
    setequal( res, c("char", "i", "data1", "data2") )
  })
})

context("rtext char_data_set()") # ========================================================

test_that("rtext char_data_set() works", {
  expect_true({
    dings <- rtext$new("123")
    rtext:::dim2(dings$char_data_get())==2
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("x1",1)
    rtext:::dim2(dings$char_data_get(x="x1"))==3
  })
  expect_true({
    dings <- rtext$new("123")
    rtext:::dim1(dings$char_data_get(x="x1"))==3
  })
  expect_true({
    dings <- rtext$new("123")
    rtext:::dim2( dings$char_data_get(x=c("x1","x2","x3")) )==5
  })
  expect_error({
    dings <- rtext$new("")
    dings$char_data_get(x=c("x1","x2","x3"))
  }, NA)
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
        structure(list( i = c(1, 2, 3, 4, 6, 7, 8, 9, 10), char = c("a", "b", "c", "d", "f", "g", "h", "i",  "j"), pimpf = c(1, 1, NA,  NA, 1, 1, NA, NA, NA), pompf = c(1, 1, 1, 1, NA, 1, 1, 1, 1)), .Names = c("i","char", "pimpf", "pompf"), row.names = c(NA, 9L), class = "data.frame"),
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
      dim(dings$char_data_get()),
      dim(subset(data.frame(i=1, char="",var1=1), FALSE))
    )
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 3, 1)

    dings$char_data_get()
    dings$char_get()

    dings$char_delete(1, from = 1)
    identical(
      as.character(dings$char_data_get()),
      as.character(hellno::data.frame( i=2, char="3", var1=1))
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
      hellno::data.frame(i=2L, char="1", var1=1)
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
      subset(hellno::data.frame(i=1L, char="", var1=1), FALSE)
    )
  })
  expect_true({
    dings <- rtext$new("123")
    dings$char_data_set("var1", 1, 1)
    dings$char_replace(1,1,"a")
    dings$char_get()
    identical(
      dings$char_data_get(),
      subset(hellno::data.frame(i=1L, char="", var1=1), FALSE)
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
      c(1,"3",1)
    )
  })
})


