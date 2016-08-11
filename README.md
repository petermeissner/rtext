
<br><br> **Status**

*getting stable*

[![Travis-CI Build Status](https://travis-ci.org/petermeissner/rtext.svg?branch=master)](https://travis-ci.org/petermeissner/rtext) [![codecov](https://codecov.io/gh/petermeissner/rtext/branch/master/graph/badge.svg)](https://codecov.io/gh/petermeissner/rtext/tree/master/R) [![CRAN version](http://www.r-pkg.org/badges/version/rtext)](https://cran.r-project.org/package=rtext)

R6 Objects for Text and Data
============================

<br><br> **Version**

0.1.14.90000 <br> 2016-08-11

<br><br> **Description**

For natural language procesing and analysis or qualitative text coding structures which provide a way to bind together text and text data are fundamental. The package provides such a structure and accompanying methods in form of R6 objects. The 'rtext' class allows for text handling and text coding (character or regex based) including data updates on text transformations as well as aggregation on various levels. Furthermore, the usage of R6 enables inheritance and passing by reference which should enable 'rtext' instances to be used as backend for R based graphical text editors or text coding GUIs.

<br><br> **License**

MIT + file LICENSE <br>Peter Meissner \[aut, cre\], Ulrich Sieberer \[cph\], University of Konstanz \[cph\]

<br><br> **Citation**

Meißner P (2016). *rtext*. R package version 0.1.14.90000, &lt;URL: <https://github.com/petermeissner/rtext>&gt;.

Sieberer U, Meißner P, Keh J and Müller W (2016). "Mapping and Explaining Parliamentary Rule Changes in Europe: A Research Program." *Legislative Studies Quarterly*, *41*(1), pp. 61-88. ISSN 1939-9162, doi: 10.1111/lsq.12106 (URL: <http://doi.org/10.1111/lsq.12106>), &lt;URL: <http://dx.doi.org/10.1111/lsq.12106>&gt;.

<br><br> **BibTex for citing**

<code style="white-space:normal;"> @Manual{Meissner2016, title = {rtext}, author = {Peter Meißner}, year = {2016}, note = {R package version 0.1.14.90000}, url = {<https://github.com/petermeissner/rtext>}, }

@Article{Sieberer2016, title = {Mapping and Explaining Parliamentary Rule Changes in Europe: A Research Program}, author = {Ulrich Sieberer and Peter Meißner and Julia F. Keh and Wolfgang C. Müller}, journal = {Legislative Studies Quarterly}, volume = {41}, number = {1}, issn = {1939-9162}, url = {<http://dx.doi.org/10.1111/lsq.12106>}, doi = {10.1111/lsq.12106}, pages = {61--88}, year = {2016}, abstract = {We outline a comprehensive research program on institutional reforms in European parliaments. Original data show that parliamentary rules in Western European parliaments have been changed frequently and massively during the period from 1945 to 2010 suggesting that actors use institutional reforms as a distinct strategy to pursue their substantive goals. We discuss how institutional instability affects existing theoretical and empirical arguments about institutional effects. Furthermore, we present four ideal-typical approaches to analyzing rule changes, present new software tools for identifying and coding changes in large text corpora, and demonstrate their usefulness for valid measurement of the overall change between subsequent text versions.}, } </code>

<br><br> **Installation**

(stable) development version

``` r
install.packages("stringb", repos="https://petermeissner.github.io/drat")    
install.packages("rtext",   repos="https://petermeissner.github.io/drat")    
library(rtext)
```

<br><br> **Example Usage**

<br><br> *... starting up ...*

``` r
library(rtext)
```

    ## Loading required package: stringb

<br><br>*... creating a text object ...*

``` r
# initialize (with text or file)
quote_text <- "Outside of a dog, a book is man's best friend. Inside of a dog it's too dark to read."
quote <- rtext$new(text = quote_text)
```

    ## rtext : initializing

<br><br>*... setting and getting data ...*

``` r
# add some data
quote$char_data_set("first", 1, TRUE)
quote$char_data_set("last", quote$char_length(), TRUE)

# get the data
quote$char_data_get()
```

    ##    i char first last
    ## 1  1    O  TRUE   NA
    ## 2 85    .    NA TRUE

<br><br>*... text transformation and data update ...*

``` r
# transform text
quote$char_add("[this is an insertion] \n", 47)

# get the data again (see, the data moved along with the text)
quote$text_get()
```

    ## [1] "Outside of a dog, a book is man's best friend. [this is an insertion] \nInside of a dog it's too dark to read."

``` r
quote$char_data_get()
```

    ##     i char first last
    ## 1   1    O  TRUE   NA
    ## 2 109    .    NA TRUE

<br><br>*... using regular expression for setting data ...*

``` r
# do some convenience coding (via regular expressions)
quote$char_data_set_regex("dog_friend", "dog", "dog")
quote$char_data_set_regex("dog_friend", "friend", "friend")
quote$char_data_get()
```

    ##      i char first last dog_friend
    ## 1    1    O  TRUE   NA       <NA>
    ## 2   14    d    NA   NA        dog
    ## 3   15    o    NA   NA        dog
    ## 4   16    g    NA   NA        dog
    ## 5   40    f    NA   NA     friend
    ## 6   41    r    NA   NA     friend
    ## 7   42    i    NA   NA     friend
    ## 8   43    e    NA   NA     friend
    ## 9   44    n    NA   NA     friend
    ## 10  45    d    NA   NA     friend
    ## 11  84    d    NA   NA        dog
    ## 12  85    o    NA   NA        dog
    ## 13  86    g    NA   NA        dog
    ## 14 109    .    NA TRUE       <NA>

<br><br>*... data aggregation via regex ...*

``` r
quote$tokenize_data_regex(regex="(dog)|(friend)", non_token = TRUE, join = "full")
```

    ## [1] from     to       token    is_token token_i 
    ## <0 rows> (or 0-length row.names)

<br><br>*... data aggregation by words ...*

``` r
quote$tokenize_data_words(non_token = TRUE, join="full")
```

    ## [1] from     to       token    is_token token_i 
    ## <0 rows> (or 0-length row.names)

<br><br>*... data aggregation by lines ...*

``` r
quote$tokenize_data_lines()
```

    ## [1] from     to       token    is_token token_i 
    ## <0 rows> (or 0-length row.names)
