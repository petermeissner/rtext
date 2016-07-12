#### diffrproject ==============================================================

context("\ndiffrproject") # ====================================================



context("diffrproject init") # =================================================

test_that("diffrproject can be created", {
  expect_true({
    dp <- diffrproject$new()
    "diffrproject"  %in% class(dp)
  })
})



context("diffrproject text_add") # ===============================

test_that("texts can be added", {
  expect_error({
    dp <- diffrproject$new()
    dp$text_add("")
  })
  expect_error({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
  }, NA)
  expect_error({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""), name = 1)
  }, NA)
  expect_error({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""), name = "a")
  }, NA)
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_add(rtext = rtext$new(""), name = "b")
    dp$text_add(rtext = rtext$new(""), name = "c")
    dp$text_add(rtext = rtext$new(""), name = "a")
    length(dp$texts) == 3
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    length(dp$texts) == 4
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_add(rtext = rtext$new(""), name = "b")
    dp$text_add(rtext = rtext$new(""), name = "c")
    dp$text_add(rtext = rtext$new(""), name = "a")
    length(dp$texts) == 7
  })
})


test_that("names and ids are unique", {
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_add(rtext = rtext$new(""), name = "b")
    dp$text_add(rtext = rtext$new(""), name = "c")
    dp$text_add(rtext = rtext$new(""), name = "a")
    all(names(dp$texts) == unique(names(dp$texts)))
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new("a"))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new("123"), name = "a")
    dp$text_add(rtext = rtext$new(""), name = "b")
    dp$text_add(rtext = rtext$new("123"), name = "c")
    dp$text_add(rtext = rtext$new("123"), name = "a")
    ids <- vapply(dp$texts, `[[`, "", "id")
    all(ids == unique(ids))
  })
})

context("diffrproject text_delete") # ===============================

test_that("texts can be deleted", {
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_add(rtext = rtext$new(""), name = "b")
    dp$text_add(rtext = rtext$new(""), name = "c")
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    length(dp$texts) == 0
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_add(rtext = rtext$new(""), name = "b")
    dp$text_add(rtext = rtext$new(""), name = "c")
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_delete(1)
    length(dp$texts) == 6
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_add(rtext = rtext$new(""), name = "b")
    dp$text_add(rtext = rtext$new(""), name = "c")
    dp$text_add(rtext = rtext$new(""), name = "a")
    dp$text_delete("b")
    dp$text_delete("b")
    length(dp$texts) == 6
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new("1"))
    dp$text_add(rtext = rtext$new("2"))
    dp$text_add(rtext = rtext$new("3"))
    dp$text_add(rtext = rtext$new("4"), name = "a")
    ID <- dp$texts$a$id
    dp$text_add(rtext = rtext$new("5"))
    dp$text_add(rtext = rtext$new("6"), name = "b")
    dp$text_add(rtext = rtext$new("7"), name = "c")
    dp$text_delete(id=ID)
    !("a"  %in% names(dp$texts))
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""), name = "a")
    ID <- dp$texts$a$id
    dp$text_add(rtext = rtext$new(""))
    dp$text_add(rtext = rtext$new(""), name = "b")
    dp$text_add(rtext = rtext$new(""), name = "c")
    dp$text_delete(id=ID)
    !("a"  %in% names(dp$texts))
  })
})











