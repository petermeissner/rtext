# README


# rtext

A package with R6 object definitions for handling text and data


<br><br>**Description**

TBD



<br><br>**Status**

*unstable* - in wild developement with fuRiouS rEstRucturINg and biG biG pOKing

[![Travis-CI Build Status](https://travis-ci.org/petermeissner/rtext.svg?branch=master)](https://travis-ci.org/petermeissner/rtext)
[![codecov](https://codecov.io/gh/petermeissner/rtext/branch/master/graph/badge.svg)](https://codecov.io/gh/petermeissner/rtext/tree/master/R)
[![CRAN version](http://www.r-pkg.org/badges/version/stringb)](https://cran.r-project.org/package=rtext)


<br><br>**Citation**



Meißner P (2016). _rtext_. R package version 0.1.11.90000, <URL:
https://github.com/petermeissner/rtext>.

Sieberer U, Meißner P, Keh J and Müller W (2016). "Mapping and
Explaining Parliamentary Rule Changes in Europe: A Research
Program." _Legislative Studies Quarterly_, *41*(1), pp. 61-88.
ISSN 1939-9162, doi: 10.1111/lsq.12106 (URL:
http://doi.org/10.1111/lsq.12106), <URL:
http://dx.doi.org/10.1111/lsq.12106>.

<br><br>**BibTex for citing**

<code style="white-space:normal;">
@Manual{Meissner2016,
  title = {rtext},
  author = {Peter Meißner},
  year = {2016},
  note = {R package version 0.1.11.90000},
  url = {https://github.com/petermeissner/rtext},
}

@Article{Sieberer2016,
  title = {Mapping and Explaining Parliamentary Rule Changes in Europe: A Research Program},
  author = {Ulrich Sieberer and Peter Meißner and Julia F. Keh and Wolfgang C. Müller},
  journal = {Legislative Studies Quarterly},
  volume = {41},
  number = {1},
  issn = {1939-9162},
  url = {http://dx.doi.org/10.1111/lsq.12106},
  doi = {10.1111/lsq.12106},
  pages = {61--88},
  year = {2016},
  abstract = {We outline a comprehensive research program on institutional reforms in European parliaments. Original data show that parliamentary rules in Western European parliaments have been changed frequently and massively during the period from 1945 to 2010 suggesting that actors use institutional reforms as a distinct strategy to pursue their substantive goals. We discuss how institutional instability affects existing theoretical and empirical arguments about institutional effects. Furthermore, we present four ideal-typical approaches to analyzing rule changes, present new software tools for identifying and coding changes in large text corpora, and demonstrate their usefulness for valid measurement of the overall change between subsequent text versions.},
}
</code>



<br><br>**Installation**


```r
  devtools::install_github("petermeissner/stringb")    
  devtools::install_github("petermeissner/rtext")
  library(rtext)
```


    

<br><br>**Example Usage**

<br><br>*... starting up ...*


```r
library(stringb)
library(rtext)
options("width"=120)
```


<br><br>*... creating a text object ...*

```r
# initialize (with text or file)
quote <- 
  rtext$new(
    text = "Outside of a dog, a book is man's best friend. Inside of a dog it's too dark to read."
  )
```

```
## rtext : initializing
```

<br><br>*... setting and getting data ...*

```r
# add some data
quote$char_data_set("first", 1, TRUE)
quote$char_data_set("last", quote$char_length(), TRUE)

# get the data
quote$char_data_get()
```

```
##   char  i first last
## 1    O  1  TRUE   NA
## 2    . 85    NA TRUE
```


<br><br>*... text transformation and data update ...*

```r
# transform text
quote$char_add("[this is an insertion] \n", 47)

# get the data again (see, the data moved along with the text)
quote$text_get()
```

```
## [1] "Outside of a dog, a book is man's best friend. [this is an insertion] \nInside of a dog it's too dark to read."
```

```r
quote$char_data_get()
```

```
##   char   i first last
## 1    O   1  TRUE   NA
## 2    . 109    NA TRUE
```


<br><br>*... using regular expression for setting data ...*

```r
# do some convenience coding (via regular expressions)
quote$char_data_set_regex("dog_friend", "dog", "dog")
quote$char_data_set_regex("dog_friend", "friend", "friend")
quote$char_data_get()
```

```
##    char   i first last dog_friend
## 1     O   1  TRUE   NA       <NA>
## 3     d  14    NA   NA        dog
## 4     o  15    NA   NA        dog
## 5     g  16    NA   NA        dog
## 11    f  40    NA   NA     friend
## 21    r  41    NA   NA     friend
## 31    i  42    NA   NA     friend
## 41    e  43    NA   NA     friend
## 51    n  44    NA   NA     friend
## 61    d  45    NA   NA     friend
## 6     d  84    NA   NA        dog
## 7     o  85    NA   NA        dog
## 8     g  86    NA   NA        dog
## 2     . 109    NA TRUE       <NA>
```

<br><br>*... data aggregation via regex ...*

```r
quote$tokenize_data_regex(regex="(dog)|(friend)", non_token = TRUE, join = "full")
```

```
##   token_i from  to                                   token is_token first last dog_friend
## 1       1    1  13                           Outside of a      TRUE  TRUE   NA       <NA>
## 2       2   14  16                                     dog    FALSE    NA   NA        dog
## 3       3   17  39                 , a book is man's best      TRUE    NA   NA       <NA>
## 4       4   40  45                                  friend    FALSE    NA   NA     friend
## 5       5   46  83 . [this is an insertion] \nInside of a      TRUE    NA   NA       <NA>
## 6       6   84  86                                     dog    FALSE    NA   NA        dog
## 7       7   87 109                  it's too dark to read.     TRUE    NA TRUE       <NA>
```

<br><br>*... data aggregation by words ...*

```r
quote$tokenize_data_words(non_token = TRUE, join="full")
```

```
##    token_i from  to     token is_token first last dog_friend
## 1        1    1   7   Outside     TRUE  TRUE   NA       <NA>
## 2        2    8   8              FALSE    NA   NA       <NA>
## 3        3    9  10        of     TRUE    NA   NA       <NA>
## 4        4   11  11              FALSE    NA   NA       <NA>
## 5        5   12  12         a     TRUE    NA   NA       <NA>
## 6        6   13  13              FALSE    NA   NA       <NA>
## 7        7   14  16       dog     TRUE    NA   NA        dog
## 8        8   17  18        ,     FALSE    NA   NA       <NA>
## 9        9   19  19         a     TRUE    NA   NA       <NA>
## 10      10   20  20              FALSE    NA   NA       <NA>
## 11      11   21  24      book     TRUE    NA   NA       <NA>
## 12      12   25  25              FALSE    NA   NA       <NA>
## 13      13   26  27        is     TRUE    NA   NA       <NA>
## 14      14   28  28              FALSE    NA   NA       <NA>
## 15      15   29  31       man     TRUE    NA   NA       <NA>
## 16      16   32  32         '    FALSE    NA   NA       <NA>
## 17      17   33  33         s     TRUE    NA   NA       <NA>
## 18      18   34  34              FALSE    NA   NA       <NA>
## 19      19   35  38      best     TRUE    NA   NA       <NA>
## 20      20   39  39              FALSE    NA   NA       <NA>
## 21      21   40  45    friend     TRUE    NA   NA     friend
## 22      22   46  48       . [    FALSE    NA   NA       <NA>
## 23      23   49  52      this     TRUE    NA   NA       <NA>
## 24      24   53  53              FALSE    NA   NA       <NA>
## 25      25   54  55        is     TRUE    NA   NA       <NA>
## 26      26   56  56              FALSE    NA   NA       <NA>
## 27      27   57  58        an     TRUE    NA   NA       <NA>
## 28      28   59  59              FALSE    NA   NA       <NA>
## 29      29   60  68 insertion     TRUE    NA   NA       <NA>
## 30      30   69  71      ] \n    FALSE    NA   NA       <NA>
## 31      31   72  77    Inside     TRUE    NA   NA       <NA>
## 32      32   78  78              FALSE    NA   NA       <NA>
## 33      33   79  80        of     TRUE    NA   NA       <NA>
## 34      34   81  81              FALSE    NA   NA       <NA>
## 35      35   82  82         a     TRUE    NA   NA       <NA>
## 36      36   83  83              FALSE    NA   NA       <NA>
## 37      37   84  86       dog     TRUE    NA   NA        dog
## 38      38   87  87              FALSE    NA   NA       <NA>
## 39      39   88  89        it     TRUE    NA   NA       <NA>
## 40      40   90  90         '    FALSE    NA   NA       <NA>
## 41      41   91  91         s     TRUE    NA   NA       <NA>
## 42      42   92  92              FALSE    NA   NA       <NA>
## 43      43   93  95       too     TRUE    NA   NA       <NA>
## 44      44   96  96              FALSE    NA   NA       <NA>
## 45      45   97 100      dark     TRUE    NA   NA       <NA>
## 46      46  101 101              FALSE    NA   NA       <NA>
## 47      47  102 103        to     TRUE    NA   NA       <NA>
## 48      48  104 104              FALSE    NA   NA       <NA>
## 49      49  105 108      read     TRUE    NA   NA       <NA>
## 50      50  109 109         .    FALSE    NA TRUE       <NA>
```

<br><br>*... data aggregation by lines ...*

```r
quote$tokenize_data_lines()
```

```
##   token_i from  to                                                                  token is_token first last
## 1       1    1  70 Outside of a dog, a book is man's best friend. [this is an insertion]      TRUE    NA   NA
## 2       2   72 109                                 Inside of a dog it's too dark to read.     TRUE    NA   NA
##   dog_friend
## 1     friend
## 2        dog
```

   

    
    
    
    
    
    
    
    
    
    
