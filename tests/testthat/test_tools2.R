#### tools =====================================================================
context("\ntools") # ===========================================================

context("tools modus")

test_that("easy examples work properly", {
  expect_true(  modus(1)==1 )
  expect_true(  modus(2)==2 )
  expect_true(  modus(1:10, warn = FALSE) == 1 )
  expect_true(  modus(10:1, warn = FALSE) ==10 )
  expect_warning( modus(c(1,1,2,2)) )
}
)



context("tools which_token()")

test_that("easy examples work properly", {
  expect_true(  which_token( x =        1, y1 =        1, y2 =           1 )  ==        1 )
  expect_true(  which_token( x =        2, y1 =   c(2,1), y2 =      c(2,1) )  ==        1 )
  expect_true(  which_token( x =        1, y1 =   c(2,1), y2 =      c(2,1) )  ==        2 )
  expect_equal( which_token( x =      1:2, y1 =   c(2,1), y2 =      c(2,1) ),      c(2,1) )
  expect_equal( which_token( x = c(7,2,4), y1 = c(1,3,7), y2 = c(2,6,2000) ),    c(3,1,2) )
  expect_equal( which_token( x =      1:4, y1 = c(1,3,7), y2 = c(2,6,2000) ), c(1,1,2,2))
  expect_true(  is.na(which_token( x =     2001, y1 = c(1,3,7), y2 = c(2,6,2000) ))       )
}
)



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


