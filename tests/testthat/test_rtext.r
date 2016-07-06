#### rtext ==============================================================================
context("\nrtext") # =================================================

context("rtext token_data_get") # ========================================================

test_that("rtext token_data_get default behaviour makes sense", {
  expect_true({
      dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
      dings$token_data_get()
      TRUE
  })
  expect_true({
    dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
    dings$char_data_set("pimpf", 1:30, 2)
    dings$char_data_set("pompf", 1:30, 1:30)
    dings$token_data_get()
    TRUE
  })
  expect_true({
    dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
    dings$char_data_set("pimpf", 1:30, 2)
    all(names(dings$token_data_get()) == c("token_i", "pimpf"))
  })
  expect_true({
    dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
    dings$char_data_set("pimpf", 1:30, 2)
    dings$char_data_set("pompf", 1:30, 1:30)
    all(names(dings$token_data_get()) == c("token_i", "pimpf", "pompf"))
  })
  expect_true({
    dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
    dings$char_data_set("pimpf", 1:30, 2)
    dings$char_data_set("pompf", 1:30, 1:30)
    dings$char_data_set("pimpf", 31, 4)
    all(names(dings$token_data_get()) == c("token_i", "pimpf", "pompf"))
  })
})

test_that("rtext token_data_get() user supplied functions work", {
  expect_true({
    dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
    dings$token_data_get(FUN="mean")
    TRUE
  })
  expect_true({
    dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
    dings$char_data_set("pimpf", 1:30, 2)
    all(names(dings$token_data_get(FUN="mean")) == c("token_i", "pimpf"))
    TRUE
  })
  expect_true({
    dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
    dings$char_data_set("pimpf", 1:30, 2)
    all(dings$token_data_get(FUN="min")$pimpf==2)
  })
  expect_true({
    dings <- rtext$new(text=text_snippet(text_read(dp_tf(1))))
    dings$char_data_set("pimpf", 1:30, 1:30)
    all(dings$token_data_get(FUN="mean")$pimpf!=2)
  })
})


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

context("rtext code") # ========================================================

test_that("rtext code", {
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
    identical(
      dings$char_data_get(),
      structure(list(char = c("a", "b", "c", "d", "f", "g", "h", "i",  "j"), i = c(1, 2, 3, 4, 6, 7, 8, 9, 10), pimpf = c(1, 1, NA,  NA, 1, 1, NA, NA, NA), pompf = c(1, 1, 1, 1, NA, 1, 1, 1, 1)), .Names = c("char",  "i", "pimpf", "pompf"), row.names = c(NA, 9L), class = "data.frame")
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






context("rtext save and load") # ===============================================
test_that("rtext save", {
  expect_error({
    dings <- rtext$new(
      text="1234567890"
    )
    dings$save()
  })
  expect_error({
    dings <- rtext$new(
      text="1234567890",
      save_file=tempfile()
    )
    dings$save()
  }, NA)
  expect_error({
    dings <-
      rtext$new(
        text_file=dp_tf(5),
        save_file=tempfile()
      )
    dings$save()
  }, NA)
  expect_error({
    dings <-
      rtext$new(
        text="1234567890",
        text_file=dp_tf(1),
        save_file=tempfile()
      )
    dings$save()
  }, NA)
})

test_that("rtext load is same as save", {
  expect_error({
    save_file <- tempfile(fileext = "Rdata")
    dings <-
      rtext$new( text="1234567890" )
    dings$save(file = save_file)
    dings$load()
  })

  expect_error({
    save_file <- tempfile(fileext = "Rdata")
    dings <-
      rtext$new( text="1234567890" )
    dings$save(file = save_file)
    dings$load(save_file)
  },NA)

  expect_error({
    save_file <- tempfile(fileext = "Rdata")
    dings <-
      rtext$new( text="1234567890" )
    dings$save(file = save_file)
    dings$load(file = save_file)
  },NA)

  expect_true({
    save_file <- tempfile(fileext = "Rdata")
    dings <-
      rtext$new(
        text      = "1234567890",
        text_file = dp_tf(5),
        tokenizer = function(x){strsplit(x,"\n")},
        encoding  = "latin1",
        id        = "bollocks",
        save_file = tempfile()
      )

    dongs <- dings$clone()
    dongs$save(file = save_file)

    dings <- rtext$new( text="" )
    dings$load(save_file)

    all(
      dings$encoding             == dongs$encoding,
      dings$id                   == dongs$id,
      dings$sourcetype           == dongs$sourcetype,
      as.character(dings$info()) == as.character(dongs$info()),
      identical(dings$save_file, dongs$save_file),
      identical(dings$text_file, dongs$text_file),
      deparse(dings$tokenizer)   == deparse(dongs$tokenizer),
      dings$text_get(Inf)        == dongs$text_get(Inf),
      dings$char_get(Inf)        == dongs$char_get(Inf)
    )
  })

  expect_true({
    save_file <- tempfile(fileext = "Rdata")
    dings     <- rtext$new(save_file=save_file)
    dings$save()
    tmp_env   <-  new.env(parent = emptyenv())
    tmp <- load_into(save_file)

    all(
      !is.null(tmp[[1]]$session_info$dp_version),
      !is.null(tmp[[1]]$session_info$r_version)
    )
  })
  expect_true({
    save_file <- tempfile(fileext = "Rdata")
    dings     <- rtext$new(text_file=dp_tf(4), encoding="latin1")
    dings$save(save_file)
    dongs <- rtext$new()$load(save_file)

    all(dings$text_get() == dongs$text_get())
  })
})


context("rtext hash") # ========================================================
test_that("rtext data_hash does not change on char manipulation", {
  expect_true({
    dings <- rtext$new(text="1234567890")
    hash1 <- dings$data_hash()
    hash2 <- dings$data_hash()
    dings$char_add("a")
    hash3 <- dings$data_hash()
    dings$char_delete(1)
    hash4 <- dings$data_hash()
    all.equal(hash1, hash2, hash3, hash4)
  })
})
test_that("rtext text_hash does not change on subsequent calls", {
  expect_true({
    dings <- rtext$new(text="1234567890")
    hash1 <- dings$text_hash()
    char1 <- dings$char_get(raw=TRUE)
    dings$char_add("")
    hash2 <- dings$text_hash()
    char2 <- dings$char_get(raw=TRUE)
    hash1 == hash2
  })
})
test_that("rtext text_hash does change on char manipulation", {
  expect_true({
    dings <- rtext$new(text="1234567890")
    hash1 <- dings$text_hash()
    char1 <- dings$char_get(raw=TRUE)
    dings$char_add("a")
    hash2 <- dings$text_hash()
    dings$char_delete(1)
    char3 <- dings$char_get(raw=TRUE)
    hash3 <- dings$text_hash()
    hash1 != hash2 & hash2 != hash3 & hash3 == hash1
  })
})




context("rtext init") # ========================================================
test_that("rtext initialization", {
  expect_error( rtext$new(), NA)
  expect_error( rtext$new(NULL), NA)
  expect_error( rtext$new(""), NA)
  expect_error( rtext$new(text_file=dp_tf(1)), NA)
  expect_error( rtext$new(text="", text_file=dp_tf(1)), NA)
  expect_error( rtext$new(text=readLines(dp_tf(1))), NA)
})



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


context("rtext hash_text") # ===================================================
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


















