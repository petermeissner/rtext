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
})











