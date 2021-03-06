#' rtext_base : basic workhorse for rtext
#'
#' @docType class
#'
#' @name rtext_base
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{R6Class}}
#'
#' @format \code{\link{R6Class}} object.
#'
#' @seealso \code{\link{rtext}}
#'
rtext_base <-
  R6::R6Class(

    #### misc ====================================================================
    classname    = "rtext_base",
    active       = NULL,
    inherit      = R6_rtext_extended,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('rtext'),



    #### private =================================================================
    private = list(
      text = function(){
        paste0(private$char, collapse = "")
      },

      char                = character(0),
      char_data           = list()
    ),



    #### public ==================================================================
    public = list(


      #### puplic data fields ====================================================
      text_file  = as.character(NA),
      encoding   = as.character(NA),
      sourcetype = as.character(NA),
      id         = NULL,
      save_file  = {a <- NA; a <- as.character(a); a},

      #### [ initialize ] #### ...................................................
      initialize =
        function(
          text        = NULL,
          text_file   = NULL,
          encoding    = "UTF-8",
          id          = NULL,
          save_file   = NULL,
          verbose     = TRUE
        )
        {

          ##### Saving verbose option
          self$options$verbose <- verbose

          ##### Stating what is done
          self$message("initializing")

          ##### read in text // set field: sourcetype
          if(is.null(text) & is.null(text_file)){ # nothing at all
            private$char <- ""
            self$sourcetype <- "empty"
          }else if(is.null(text) & !is.null(text_file)){ # read from text_file
            private$char <- text_read(text_file, encoding = encoding, tokenize = "")
            self$sourcetype <- "text_file"
          }else{ # take text as supplied
            private$char <-
              unlist(strsplit(paste0(iconv(text, encoding, "UTF-8"), collapse = "\n"),""))
            self$sourcetype <- "text"
          }

          ##### set field: text_file
          if( !is.null(text_file) ){
            self$text_file <- text_file
          }

          ##### set field: save_file
          if( !is.null(save_file) ){
            self$save_file <- save_file
          }

          ##### Encoding
          Encoding(private$char) <- "UTF-8"
          self$encoding <- "UTF-8"

          ##### ID
          if( is.null(id) ){
            self$id <- rtext_hash(self)
          }else{
            self$id <- id
          }

          ##### Hashing again
          private$hash()
        }
      ,

      #### methods ===============================================================

      # info
      info = function(){
        res <-
          list(
            text_file  = self$text_file,
            character  = length(private$char),
            encoding   = self$encoding,
            sourcetype = self$sourcetype
          )
        return(res)
      },

      #### [ text_show ] #### ....................................................
      text_show = function(length=500, from=NULL, to=NULL, coll=FALSE, wrap=FALSE){
        text_show(x=self$text_get(Inf), length=length, from=from, to=to, coll=coll, wrap=wrap)
      },

      #### [ text_get ] ..........................................................
      text_get = function(length=Inf, from=NULL, to=NULL, split=NULL){
        res <- rtext_get_character(chars=private$char, length=length, from=from, to=to)
        res <- paste0(res, collapse = "")
        Encoding(res) <- self$encoding
        if( !is.null(split) ){
          res <- unlist(strsplit(res, split = split))
          Encoding(res) <- self$encoding
        }
        return(res)
      },

      #### [ text_get_lines ] ..........................................................
      text_get_lines = function(length=Inf, from=NULL, to=NULL){
          # get text
          tmp_text  <- self$text_get(length=length, from=from, to=to)
          # split/tokenize
          tmp       <- text_tokenize(tmp_text, "\n")
          # gather other data
          tmp$is_token <- NULL
          line <- seq_along(tmp$from)
          n    <- nchar(tmp$token)
          #return
          return(
            data.frame(line, n, from=tmp$from, to=tmp$to, text=tmp$token)
          )
      },

      #### [ char_get ] #### .....................................................
      char_get = function(length=Inf, from=NULL, to=NULL, raw=FALSE){
        if(raw | identical(length, TRUE) ){
          res <- private$char
          Encoding(res) <- self$encoding
          return(res)
        }
        res <- get_vector_element(vec=private$char, length=length, from=from, to=to)
        Encoding(res) <- self$encoding
        return(res)
      },

      #### [ char_add ] #### .....................................................
      char_add = function(what=NULL, after=NULL){
        what        <- enc2utf8(what)
        what        <- unlist(strsplit(what,""))
        if( is.null(after) ) {
          private$char <- c(private$char, what)
        }else if ( after==0 ) {
          private$char <- c(what, private$char)
          # update char_data$i
          for( name_i in seq_along(names(private$char_data)) ){
            name <- names(private$char_data)[name_i]
            private$char_data[[name]]$i <- private$char_data[[name]]$i + length(what)
          }
        }else{
          index  <- seq_along(private$char)
          part1  <- private$char[index <= after]
          part2  <- private$char[index >  after]
          private$char <- c( part1, what, part2)
          # update char_data$i
          for( name_i in seq_along(names(private$char_data)) ){
            name <- names(private$char_data)[name_i]
            iffer <- private$char_data[[name]]$i > after
            private$char_data[[name]]$i[iffer] <- private$char_data[[name]]$i[iffer] + length(what)
          }
        }
        # necessary updates
        private$hash("char")
        # return for piping
        invisible(self)
      },

      #### [ char_delete ] #### ..................................................
      char_delete = function(n=NULL, from=NULL, to=NULL){
        non_deleted  <- vector_delete(x = seq_along(private$char), n=n, from=from, to=to)
        private$char <- vector_delete(x = private$char, n=n, from=from, to=to)
        # update char_data$i (drop deletd data, update index)
        new_index           <- seq_along(non_deleted)
        for( name_i in seq_along(names(private$char_data)) ){
          name <- names(private$char_data)[name_i]
          private$char_data[[name]] <-
            subset(private$char_data[[name]], private$char_data[[name]]$i %in% non_deleted)
          private$char_data[[name]]$i <- new_index[match(private$char_data[[name]]$i, non_deleted)]
        }
        # necessary updates
        private$hash(c("char", "char_data"))
        # return for piping
        invisible(self)
      },

      #### [ char_replace ] #### .................................................
      char_replace = function(from=NULL, to=NULL, by=NULL){
        # check input
        stopifnot( !is.null(from), !is.null(to), !is.null(by) )
        by <- enc2utf8(by)
        # doing-duty-to-do
        index <- seq_along(private$char)
        private$char <-
          c(
            private$char[index < from],
            unlist(strsplit(by, "")),
            private$char[index > to]
          )
        # updata char_data
        for( name_i in seq_along(names(private$char_data)) ){
          name <- names(private$char_data)[name_i]
          private$char_data[[name]] <-
            subset(
              private$char_data[[name]],
              private$char_data[[name]]$i < from | private$char_data[[name]]$i > to
            )
          iffer <-
            private$char_data[[name]]$i > to
          private$char_data[[name]]$i[iffer] <-
            private$char_data[[name]]$i[iffer] + nchar(by) - to - from + 1
        }
        # necessary updates
        private$hash("char")
        # return for piping
        invisible(self)
      },
      char_length = function(){
        length(private$char)
      },

      #### [ char_data_set ] #### ................................................
      char_data_set = function(x=NULL, i=NULL, val=NA, hl = 0){
        # check input
        if( length(i) == 0 ){
          if( is.null(private$char_data[[x]]) ){
              tmp <-
                subset(data.frame(i=1, char="", x=val), FALSE)
              names(tmp)[3] <- x
              private$char_data[[x]]  <- tmp
          }
          return(invisible(self))
        }
        stopifnot( length(x) == 1 )
        stopifnot( x != c("i", "char", "hl") )
        if( is.null(x) | is.null(i) ){
          warning("char_data_set : no sufficient information passed for x, i - nothing coded")
          invisible(self)
        }
        if( any( i > self$char_length() | any( i < 1)) ){
          stop("char_data_set : i out of bounds")
        }
        # prepare input
        if( length(val)==1 ){
          val <- rep(val, length(i))
        }
        if( length(hl)==1 ){
          hl <- rep(hl, length(i))
        }
        # check for coresponding lengths
        stopifnot( length(i) == length(val) & length(val) == length(hl) )

        # make sure there is a data frame to fill
        if( is.null(private$char_data[[x]] ) ){
          private$char_data[[x]] <-
            subset(
              data.frame(
                i    = 1L,
                hl   = 0
              ),
              FALSE
            )
        }

        # split data
        # - new i in old i and level is less or equal to new level
        # -> already coded with lower level are discarded!
        i_in_char_data  <-
          merge(
            data.frame(i=i),
            subset(private$char_data[[x]], TRUE,  c("i", "hl")),
            all.x = TRUE,
            by="i"
          )$hl <= hl
        i_in_char_data[is.na(i_in_char_data)] <- FALSE


        # - adding those not already coded
        i_not_in_char_data     <- !(i %in% private$char_data[[x]]$i)

        # assign data with i already in i
        input_to_data_matcher <-
          match(i[i_in_char_data], private$char_data[[x]]$i)

        private$char_data[[x]][input_to_data_matcher, "i"] <-
          i[i_in_char_data]

        private$char_data[[x]][input_to_data_matcher, "hl"]   <-
          hl[i_in_char_data]

        private$char_data[[x]][input_to_data_matcher, x]   <-
          val[i_in_char_data]

        # code for i not already in char_data
        add_df <-
          data.frame(
            i  = i[i_not_in_char_data],
            hl = hl[i_not_in_char_data]
          )

        add_df[[x]] <-
          val[i_not_in_char_data]

        private$char_data[[x]] <-
          rbind_fill(
            private$char_data[[x]],
            add_df
          ) %>%
          dp_arrange("i")

        # necessary updates
        private$hash("char_data")

        # return for piping
        invisible(self)
      },

      #### [ char_data_set_regex ] #### ..........................................
      char_data_set_regex = function(x=NULL, pattern=NULL, val=NA, hl=0, ...){
        found_spans <- text_locate_all(private$text(), pattern, ...)[[1]]
        found_spans <- subset(found_spans, !is.na(start) & !is.na(end))
        found_is    <- unique(as.integer(unlist(mapply(seq, found_spans$start, found_spans$end))))
        self$char_data_set(x=x, i=found_is, val=val,  hl=hl)
      },

      #### [ char_data_get ] #### ................................................
      char_data_get = function( from = 1, to = Inf, x = NULL, full=FALSE){
        if( from > length(private$char) | to < 1 | to < from ){
          return(data.frame())
        }
        # subset columns
        if( is.null(x) ){
          l_tbr <- private$char_data
        }else{
          l_tbr <- private$char_data[ x ]
          iffer <-
            vapply(
              l_tbr,
              function(x){
                if( is.null(x) ){ return(TRUE) }else{
                  if( dim1(x)==0 ){ return(TRUE) }else{
                    return(FALSE)
                }}
              },
              TRUE
            )
          l_tbr <- l_tbr[!iffer]
        }
        # something to return?
        if( length(l_tbr) == 0 ){
          res <- data.frame(i=seq(max(from, 1), min(to, length(private$char))))
        }else{
          # putting together data.frames
          l_tbr <- lapply(l_tbr, function(x){x["hl"] <- NULL;x})
          res <- Reduce(
            function(x, y){
              merge(x, y, by="i", all=TRUE)
            },
            l_tbr
          )
        }
        # subset according to: from and to
        res <- subset(res, res$i >= from & res$i <= to)
        # adding char
        char_i <- seq_along(private$char)
        iffer <- char_i >= from & char_i <= to
        char <- data.frame(char = private$char[iffer], i = char_i[iffer]  )
        res <-
          merge(
            char,
            subset(res, res$i >= from & res$i <=to),
            by  = "i",
            all.x = full,
            all.y = TRUE
          )
        # adding xs not found
        x_amiss <- x[!(x %in% names(res))]
        for( i in seq_along(x_amiss) ){
          res[x_amiss[i]] <- rep(NA, dim1(res))
        }
        # return
        return( res )
      },

      #### [ hash_get ] #### .....................................................
      hash_get = function(name=""){
        private$hashed(name)
      }
    )
  )





