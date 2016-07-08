#### diffrproject ==============================================================

context("\ndiffrproject") # =================================================

context("diffrproject init") # ===============================================
test_that("diffrproject can be created", {
  expect_true({
    dp <- diffrproject$new
    dp
    TRUE
  })
})













