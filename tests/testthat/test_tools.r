#### tools ==============================================================================

context("tools vector_delete()")

test_that("vector_delete works with only n as argument", {
  x <- 1L:10L
  a <- letters[1:10]
  expect_identical( vector_delete(x), x  )
  expect_identical( vector_delete(a), a  )
  expect_identical( vector_delete(a,10),character(0) )
  expect_identical( vector_delete(x,10),integer(0) )
  expect_identical( vector_delete(x, 5) , x[1:5] )
  expect_identical( vector_delete(a, 5) , a[1:5] )
})

test_that("vector_delete works with various arguments", {
  x <- unlist(strsplit("12345",""))
  expect_true( text_collapse(vector_delete(x, from= 1          ))=="")
  expect_true( text_collapse(vector_delete(x, from=-2          ))=="")
  expect_true( text_collapse(vector_delete(x, from= 3          ))=="12")
  expect_true( text_collapse(vector_delete(x, from= 9          ))=="12345")
  expect_true( text_collapse(vector_delete(x, to= 3            ))=="45")
  expect_true( text_collapse(vector_delete(x, to= 9            ))=="")
  expect_true( text_collapse(vector_delete(x, to=-9            ))=="12345")
  expect_true( text_collapse(vector_delete(x, to= 1            ))=="2345")
  expect_true( text_collapse(vector_delete(x, n = 1, from   =  1))=="2345")
  expect_true( text_collapse(vector_delete(x, n = 4, from   =  3))=="12")
  expect_true( text_collapse(vector_delete(x, n = 0, from   =  1))=="12345")
  expect_true( text_collapse(vector_delete(x, n = 5, from   = -2))=="345")
  expect_true( text_collapse(vector_delete(x, n = 0, to     =  1))=="12345")
  expect_true( text_collapse(vector_delete(x, n = 1, to     =  1))=="2345")
  expect_true( text_collapse(vector_delete(x, n = 9, to     =  1))=="2345")
  expect_true( text_collapse(vector_delete(x, n = 1, to     =  9))=="12345")
  expect_true( text_collapse(vector_delete(x, n = 2, to     =  6))=="1234")
  expect_true( text_collapse(vector_delete(x, from = 2, to  =  3))=="145")
  expect_true( text_collapse(vector_delete(x, from = -2, to =  3))=="45")
  expect_true( text_collapse(vector_delete(x, from =  2, to = 30))=="1")
  expect_true( text_collapse(vector_delete(x, from =  1, to =  5))=="")
  expect_true( text_collapse(vector_delete(x, from =  4, to =  4))=="1235")
  expect_true( text_collapse(vector_delete(x, from =  5, to =  4))=="12345")

  expect_true(
    rtext$new(text="12345")$char_delete(n = 0, to   = 5)$text_get()==
      rtext$new(text="12345")$char_delete(0)$text_get()
  )
  expect_true(
    rtext$new(text="12345")$char_delete(n = 3, to   = 5)$text_get()==
      rtext$new(text="12345")$char_delete(3)$text_get()
  )
})


context("tools which_token()")

test_that("", {
  expect_true( which_token(1,1,1)==1 )
  expect_true( which_token(2,c(2,1),c(2,1))==1 )
  expect_true( which_token(1,c(2,1),c(2,1))==2 )
  expect_equal( which_token(1:2,c(2,1),c(2,1)), c(2,1) )
}
)




#### text tools ================================================================

context("text_tools text_read()")

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


context("text_tools text_snippet()")

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


context("text_tools text_tokenize()")

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



