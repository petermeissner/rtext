#### diffrproject ==============================================================

context("\ndiffrproject") # ====================================================



context("diffrproject init") # =================================================

test_that("diffrproject can be created", {
  expect_true({
    dp <- diffrproject$new()
    "diffrproject"  %in% class(dp)
  })
})



context("diffrproject text_add / text_delete") # ===============================


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
    length(dp$texts) == 0
  })
})











