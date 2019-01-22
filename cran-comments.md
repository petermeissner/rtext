
## Your complaints 

- Please see the problems shown on <https://cran.r-project.org/web/checks/check_results_rtext.html>.
- Dear Peter,

I found that rtext package contains file named NA (in 
rtext/tests/testthat/NA). The file is accidentally created by test 
test_rtext_loadsave.R#6, this part

context("rtext_loadsave save") # 
===============================================

     test_that("rtext save", {
       expect_error({
         dings <- rtext_loadsave$new(
           text="1234567890"  <============ no save_file given, the 
default is NA
         )
         dings$save()

which calls later into save(file=NA_character_), which creates the file 
named "NA". This behavior of base R is accidental (and indeed 
undocumented) and I am now changing it to report an error, so your 
package will start failing the tests.

I've run all CRAN+BIOC package checks to see which packages fail with 
the change, and yours is the only one. Could you please remove the test 
(and possibly also the accidentally left NA file) from the package?



## My Actions

- fixed


## Test environments

- Win devel: https://win-builder.r-project.org/nJ623V568zUF/00check.log
- ok

- Win release: https://win-builder.r-project.org/35fxSv3a2foF/00check.log
- I get errors from time to time - every second build, one of two different errors - but not always and not always the same error
- According to Email-conversation with Kurt Hornik I should fix the bug that was caused by a change in R-devel and submit and later on fix those other bugs


- Win old: https://win-builder.r-project.org/AM3vOgT542Mq/00check.log
- ok

- Ubuntu Linux 16.04 LTS, R-release, GCC: https://travis-ci.org/petermeissner/rtext/builds
- ok

