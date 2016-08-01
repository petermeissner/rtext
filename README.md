# README


# rtext

A package with R6 object definitions for handling text and data


**Description**

TBD



**Status**

*unstable* - in wild developement with fuRiouS rEstRucturINg and biG biG pOKing

[![Travis-CI Build Status](https://travis-ci.org/petermeissner/rtext.svg?branch=master)](https://travis-ci.org/petermeissner/rtext)


**Citation**



Meißner P (2016). _rtext_. R package version 0.1.0.90000, <URL:
https://github.com/petermeissner/diffrprojects>.

Sieberer U, Meißner P, Keh J and Müller W (2016). "Mapping and
Explaining Parliamentary Rule Changes in Europe: A Research
Program." _Legislative Studies Quarterly_, *41*(1), pp. 61-88.
ISSN 1939-9162, doi: 10.1111/lsq.12106 (URL:
http://doi.org/10.1111/lsq.12106), <URL:
http://dx.doi.org/10.1111/lsq.12106>.

**BibTex for citing**

<code style="white-space:normal;">
@Manual{Meissner2016,
  title = {rtext},
  author = {Peter Meißner},
  year = {2016},
  note = {R package version 0.1.0.90000},
  url = {https://github.com/petermeissner/diffrprojects},
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



**Installation**


```r
  devtools::install_github("petermeissner/stringb")    
  devtools::install_github("petermeissner/rtext")
  library(rtext)
```


    

**Example Usage**


```r
library(rtext)
library(stringb)


# initialize (with text or file)
quote <- rtext$new(text=
"Outside of a dog, a book is man's best friend. Inside of a dog it's too dark to read.")
```

```
## rtext : initializing
```

```r
# add some data
quote$char_data_set("start", 1, TRUE)
quote$char_data_set("end", quote$char_length(), TRUE)

# get the data
quote$char_data_get()
```

```
##   char  i start  end
## 1    O  1  TRUE   NA
## 2    . 85    NA TRUE
```

```r
# transform text
quote$char_add("[this is an insertion] ", 47)

# get the data again (see, the data moved along with the text)
quote$text_get()
```

```
## [1] "Outside of a dog, a book is man's best friend. [this is an insertion] Inside of a dog it's too dark to read."
```

```r
quote$char_data_get()
```

```
##   char   i start  end
## 1    O   1  TRUE   NA
## 2    . 108    NA TRUE
```

   

    
    
    
    
    
    
    
    
    
    
