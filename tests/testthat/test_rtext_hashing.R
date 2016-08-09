


context("rtext hash") # ========================================================
test_that("rtext data_hash does not change on char manipulation", {
  expect_true({
    dings <- rtext$new(text="1234567890")
    hash1 <- dings$hash_get("data")
    hash2 <- dings$hash_get("data")
    dings$char_add("a")
    hash3 <- dings$hash_get("data")
    dings$char_delete(1)
    hash4 <- dings$hash_get("data")
    all.equal(hash1, hash2, hash3, hash4)
  })
})
test_that("rtext text_hash does not change on subsequent calls", {
  expect_true({
    dings <- rtext$new(text="1234567890")
    hash1 <- dings$hash_get("text")$text
    char1 <- dings$char_get(raw=TRUE)
    dings$char_add("")
    hash2 <- dings$hash_get("text")$text
    char2 <- dings$char_get(raw=TRUE)
    hash1 == hash2
  })
})
test_that("rtext text_hash does change on char manipulation", {
  expect_true({
    dings <- rtext$new(text="1234567890")
    hash1 <- dings$hash_get("text")$text
    char1 <- dings$char_get(raw=TRUE)
    dings$char_add("a")
    hash2 <- dings$hash_get("text")$text
    dings$char_delete(1)
    char3 <- dings$char_get(raw=TRUE)
    hash3 <- dings$hash_get("text")
    hash1 != hash2 & hash2 != hash3 & hash3 == hash1
  })
})


context("rtext hash_text") # ===================================================
test_that("rtext hash_text works", {
  expect_true({
    dings <- rtext$new(paste0(sample(letters, 100, replace = TRUE), collapse = ""))
    hash1 <- dings$hash_get("text")$text
    hash2 <- dings$hash_get("text")$text
    hash1 == hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$hash_get("text")$text
    dings$char_add("/")
    hash2 <- dings$hash_get("text")$text
    hash1 != hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$hash_get("text")$text
    dings$char_add("")
    hash2 <- dings$hash_get("text")$text
    hash1 == hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$hash_get("text")$text
    dings$char_delete()
    hash2 <- dings$hash_get("text")$text
    hash1 == hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$hash_get("text")$text
    dings$char_delete(1)
    hash2 <- dings$hash_get("text")$text
    hash1 != hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$hash_get("text")$text
    dings$char_delete(from = 1)
    hash2 <- dings$hash_get("text")$text
    hash1 != hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$hash_get("text")$text
    dings$char_delete(to = 1)
    hash2 <- dings$hash_get("text")$text
    hash1 != hash2
  })
  expect_true({
    dings <- rtext$new(text="----")
    hash1 <- dings$hash_get("text")$text
    dings$char_delete(from = 1, to = 1)
    hash2 <- dings$hash_get("text")$text
    hash1 != hash2
  })
})


