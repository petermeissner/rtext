NEWS rtext
==========================================================================


version 0.1.15 // 2016-08-10 ... 
--------------------------------------------------------------------------

* BUGFIXES

    
* FEATURE
    
    
    
* Development
    - rtext_extended : moving verbose to self$options
    - introducing self$options as general place to put options
    
    

version 0.1.15 // 2016-08-10 ... 
--------------------------------------------------------------------------

* BUGFIXES

    
* FEATURE
    - adding export functionality to SQLite databases
    
    
* Development
    - rtext_loadsave : restructuring
    
    

version 0.1.14 // 2016-08-10 ... 
--------------------------------------------------------------------------

* BUGFIXES
    - minor fixes discovered by testing:

    
* FEATURE
    - testing
    
    
* Development
    - testing testing testing
    - restructuring plot.rtext()
      
      
      

version 0.1.13 // 2016-08-08 ... 
--------------------------------------------------------------------------

* BUGFIXES
    

    
* FEATURE
    - R6_rtext_extended : ls()
    - R6_rtext_extended : hash(), hashed(), hashes
    - R6_rtext_extended : get()
    - adding second order data (data that is less important than first order 
      data) to allow for inheritance of data while not overriding first class 
      data
    
    
* Development
    - splitting rtext class into R6_rtext_extended, rtext_base and rtext and 
      using inheritance 




version 0.1.12 // 2016-08-08 ... 
--------------------------------------------------------------------------

* BUGFIXES
    

    
* FEATURE
    
    
* Development
    - restructuring internal hashing functions and data to be more streamlined


version 0.1.11 // 2016-08-04 ... 
--------------------------------------------------------------------------

* BUGFIXES
    - fixing non-split char bug
    - fixing minor issues: unnecessary exports, un-documented, ...

    
* FEATURE


* Development



version 0.1.10 // 2016-08-01 ... 
--------------------------------------------------------------------------

* BUGFIXES
    - 

    
* FEATURE
    - rtext : tokenize_data_regex()
    - rtext : tokenize_data_lines()
    - rtext : tokenize_data_words()
    
    
* Development



version 0.1.9 // 2016-06-09 ... 
--------------------------------------------------------------------------

* BUGFIXES
    - fixing which_token supressing results error

    
* FEATURE
    - rtext : char_code()     -> char_data_set()
    - rtext : char_get_code() -> char_data_get()
    - rtext : introducing tokens and token_data
    - introducing Rcpp to speed up: which_token_worker, which_token


* Development



version 0.1.8 // 2016-06-07 ... 
--------------------------------------------------------------------------

* BUGFIXES

    
* FEATURE
    - rtext : char_code()
    - rtext : char_length()


* Development



version 0.1.7 // 2016-06-06 ... 
--------------------------------------------------------------------------

* BUGFIXES
    - rtext : text_get() and char_get() would return text decently encoded as UTF-8 nut fail to tell Windows about that
    
* FEATURE



* Development



version 0.1.6 // 2016-05-14 ... 
--------------------------------------------------------------------------

* BUGFIXES
    
    
* FEATURE
    - rtext : save()
    - rtext : load()
    - rtext : text is tokenized into characters and then stored in characters
    - rtext : char_add()
    - rtext : char_delete()
    - rtext : char_replace()
    - rtext : text_hash()
    - rtext : hash_text()



* Development



version 0.1.5 // 2016-05-13 ... 
--------------------------------------------------------------------------

* BUGFIXES
    - rtext : getting tokenization on init right
    
* FEATURE
    - dp_text : !!! rename to rtext :-) !!!



* Development



version 0.1.4 // 2016-05-09 ... 
--------------------------------------------------------------------------

* BUGFIXES
    - fixing documentation and minor build check complaints 
    
* FEATURE
    - dp_text() : add tokenization to initializetion stage
 
 
    
* Development



version 0.1.3 // 2016-05-09 ... 
--------------------------------------------------------------------------


* BUGFIXES



* FEATURES
    - tools : text_tokenize()
    - tools : text_tokenize_words()
    

* Development




version 0.2.2 // 2016-04-28 ... 
--------------------------------------------------------------------------


* BUGFIXES


* FEATURES
    - dp_text : show_text()
    - dp_text : info()
    - dp_text : get_text()


* Development



version 0.1.1 // 2016-04-27 ... 
--------------------------------------------------------------------------

* FEATURES
    - dp_text : an object for text (basic layout)
    - tools : text_read() function for reading text
    - tools : text_snippet() function for getting snippet of text


* Development



version 0.1.0 // 2016-04-26 ... 
--------------------------------------------------------------------------

* START of development


    

