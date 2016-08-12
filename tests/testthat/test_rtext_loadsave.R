context("\nrtext_loadsave") # ===============================================

context("rtext_loadsave save") # ===============================================
if(TRUE | !any(grepl("test-all.R", commandArgs()))){

    test_that("rtext save", {
      expect_error({
        dings <- rtext_loadsave$new(
          text="1234567890"
        )
        dings$save()
      })
      expect_error({
        dings <- rtext_loadsave$new(
          text="1234567890",
          save_file=base::tempfile()
        )
        dings$save()
      }, NA)
      expect_error({
        dings <-
          rtext_loadsave$new(
            text_file=test_file("test_utf8.txt"),
            save_file=base::tempfile()
          )
        dings$save()
      }, NA)
      expect_error({
        dings <-
          rtext_loadsave$new(
            text="1234567890",
            text_file=test_file("rc_1.txt"),
            save_file=base::tempfile()
          )
        dings$save()
      }, NA)
    })




context("rtext_loadsave load") # ===============================================
test_that("rtext load is same as save", {
  expect_error({
    save_file <- base::tempfile(fileext = "Rdata")
    dings <-
      rtext_loadsave$new( text="1234567890" )
    dings$save(file = save_file)
    dings$load()
  })

  expect_error({
    save_file <- base::tempfile(fileext = "Rdata")
    dings <-
      rtext_loadsave$new( text="1234567890" )
    dings$save(file = save_file)
    dings$load(save_file)
  },NA)

  expect_error({
    save_file <- base::tempfile(fileext = "Rdata")
    dings <-
      rtext_loadsave$new( text="1234567890" )
    dings$save(file = save_file)
    dings$load(file = save_file)
  },NA)

  expect_true({
    save_file <- base::tempfile(fileext = "Rdata")
    dings <-
      rtext_loadsave$new(
        text      = "1234567890",
        text_file = test_file("test_utf8.txt"),
        encoding  = "latin1",
        id        = "bollocks",
        save_file = base::tempfile()
      )

    dongs <- dings$clone()
    dongs$save(file = save_file)

    dings <- rtext_loadsave$new( text="" )
    dings$load(save_file)

    all(
      dings$encoding             == dongs$encoding,
      dings$id                   == dongs$id,
      dings$sourcetype           == dongs$sourcetype,
      as.character(dings$info()) == as.character(dongs$info()),
      identical(dings$save_file, dongs$save_file),
      identical(dings$text_file, dongs$text_file),
      dings$text_get(Inf)        == dongs$text_get(Inf),
      dings$char_get(Inf)        == dongs$char_get(Inf)
    )
  })

  expect_true({
    save_file <- base::tempfile(fileext = "Rdata")
    dings     <- rtext_loadsave$new(save_file=save_file)
    dings$save()
    tmp_env   <-  new.env(parent = emptyenv())
    tmp <- load_into(save_file)

    all(
      !is.null(tmp[[1]]$session_info$dp_version),
      !is.null(tmp[[1]]$session_info$r_version)
    )
  })
  expect_true({
    save_file <- base::tempfile(fileext = "Rdata")
    dings     <- rtext_loadsave$new(text_file=test_file("test_latin1.txt"), encoding="latin1")
    dings$save(save_file)
    dongs <- rtext_loadsave$new()$load(save_file)

    all(dings$text_get() == dongs$text_get())
  })
})

}
























