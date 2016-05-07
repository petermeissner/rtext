library(diffrprojects)

dings <- dp_text$new(file=dp_tf(1))
dings$show_text(wrap = 10)
dings$get_text(10)
dings$id
dings$info()


dings <- dp_text$new(file=dp_tf(1))
dings$id
dings$info()


x      <- ".Meine Mudder, schneidet Spargelsuppe."
regex  <- "[^[:alnum:]]+"
invert <- FALSE

regex= ""

text_tokenize_old()
