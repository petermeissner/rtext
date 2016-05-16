# # get_text
# get_text = function(length=100, from=NULL, to=NULL, split=NULL){
#   # helper functions
#   bind_to_charrange <- function(x){bind_between(x, 1, length(self$chars))}
#   bind_length       <- function(x){bind_between(x, 0, length(self$chars))}
#   return_from_to    <- function(from, to, split){
#     res  <- paste0( self$chars[seq(from=from, to=to)], collapse = "")
#     if( !is.null(split) ){
#       res <- unlist(strsplit(res, split = split))
#     }
#     return(res)
#   }
#   # only length
#   if( !is.null(length) & ( is.null(from) & is.null(to) ) ){
#     length <- max(0, min(length, length(self$chars)))
#     length <- bind_length(length)
#     if(length==0){
#       return("")
#     }
#     from   <- 1
#     to     <- length
#     return(return_from_to(from, to, split))
#   }
#   # from and to (--> ignores length argument)
#   if( !is.null(from) & !is.null(to) ){
#     from <- bind_to_charrange(from)
#     to   <- bind_to_charrange(to)
#     return(return_from_to(from, to, split))
#   }
#   # length + from
#   if( !is.null(length) & !is.null(from) ){
#     if( length<=0 | from + length <=0 ){
#       return("")
#     }
#     to   <- from + length-1
#     if((to < 1 & from < 1) | (to > length(self$chars) & from > length(self$chars) )){
#       return("")
#     }
#     to   <- bind_to_charrange(to)
#     from <- bind_to_charrange(from)
#     return(return_from_to(from, to, split))
#   }
#   # length + to
#   if( !is.null(length) & !is.null(to) ){
#     if( length<=0 | to - (length-1) > length(self$chars) ){
#       return("")
#     }
#     from <- to - length + 1
#     if((to < 1 & from < 1) | (to > length(self$chars) & from > length(self$chars) )){
#       return("")
#     }
#     from <- bind_to_charrange(from)
#     to   <- bind_to_charrange(to)
#     return(return_from_to(from, to, split))
#   }
#   stop("rtext$get_text() : I do not know how to make sense of given length, from, to argument values passed")
# },
