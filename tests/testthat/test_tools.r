#### tools ==============================================================================
context("text_tools")

test_that("text_read", {
  expect_true( Encoding(text_read(dp_tf("test_latin1.txt"))) == "UTF-8")
  expect_true( Encoding(text_read(dp_tf("test_utf8.txt"))) == "UTF-8")
  expect_true( Encoding(text_read(dp_tf("test_latin1.txt"), encoding="latin1")) == "UTF-8")
  expect_true( Encoding(text_read(dp_tf("test_utf8.txt"), encoding="latin1")) == "UTF-8")
  expect_true(
    nchar(text_read(dp_tf("test_utf8.txt"), encoding="UTF-8")) ==
      nchar(text_read(dp_tf("test_latin1.txt"), encoding="latin1"))
  )
  expect_true( length(text_read(dp_tf("test_utf8.txt"), encoding="latin1"))==1 )
  expect_true( length(text_read(dp_tf("test_utf8.txt"), tokenize = "\n"))>1 )
  expect_true( length(text_read(dp_tf("test_utf8.txt"), tokenize = " "))>1 )
  expect_true( length(text_read(dp_tf("test_utf8.txt"), tokenize = function(x){strsplit(x,"")} ))>1 )
})

test_that("text_snippet", {
  tt500 <- paste(rep(0:9,50), collapse = "")
  tt100 <- paste(rep(0:9,10), collapse = "")
  ttvec <- c(tt100, tt500)

  expect_true( nchar(text_snippet(""))==0 )
  expect_true( nchar(text_snippet("", from=2))==0 )
  expect_true( nchar(text_snippet("", to=2))==0 )
  expect_true( nchar(text_snippet("", from=1, to=2))==0 )

  expect_true( nchar(text_snippet(tt100, from=1, to=2))==2 )
  expect_true( nchar(text_snippet(tt100, from=2, to=1))==0 )
  expect_true( nchar(text_snippet(tt100, from=-2, to=0))==0 )
  expect_true( nchar(text_snippet(tt100, from=1, to=-1))==0 )

  expect_true( nchar(text_snippet(tt500))==500 )

  expect_true( nchar(text_snippet(tt100))==100 )
  expect_true( nchar(text_snippet(tt100,50))==50)
  expect_true( nchar(text_snippet(tt100, 50, to=25))==25 )
  expect_true( nchar(text_snippet(tt100, 50, from=25))==50 )
  expect_true( nchar(text_snippet(tt100, 50, from=99))==2 )
  expect_true( nchar(text_snippet(tt100, 50, from=99, to=99))==1 )
  expect_true( nchar(text_snippet(tt100, from=0, to=1))==1 )

  expect_true( nchar(text_snippet(tt500, 50, from=1, to=500))==500 )

  expect_true( all(nchar(text_snippet(ttvec, 50, from=1, to=500))==c(100,500)) )
  expect_true( all(nchar(text_snippet(ttvec, 10))==c(10,10)) )
})


test_that("text_tokenize", {
  expect_true(
    dim(text_tokenize("abcdefg", regex="\\W+"))[1]==1
  )
  expect_true({
    text_tokenize(" ", regex=""); TRUE
  })
  expect_true({
    text_tokenize("", regex=""); TRUE
   })
  expect_true({
    text_tokenize("  ", regex=" "); TRUE
  })
})



