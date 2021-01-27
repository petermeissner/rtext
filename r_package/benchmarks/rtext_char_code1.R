#' text class
#'
#'
#' @docType class
#'
#' @name rtext
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{R6Class}}
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field text
#'  a single character string / character vector of length one
#'
#' @field tokens
#'  a data frame with a character vector representing the the text as tokens,
#'  and two integer vectors capturing the span of the token measured in
#'  characters starting from beginning of text
#'
#' @field file
#'  path to a file from which text was read in
#'
#' @field encoding
#'  encoding to be assumed for the text (will always be UTF-8, because.)
#'
#' @field sourcetype
#'  list of logicals that capture where the text came from when
#'  initializing object: text or file.
#'  (text=NULL, file=NULL) : "empty";
#'  (text="", file=NULL | file="") : "text";
#'  (text=NULL, file="") : "file"
#'
#'
#' @section Methods:
#' \describe{
#'  \item{
#'    \code{
#'    new(text=NULL, file=NULL, tokenize = "\n", encoding="UTF-8", id=NULL)
#'  }}{
#'    Method called when initialising object via
#'    \code{robotstxt$new()}. Needs either \code{text} or \code{domain} to be
#'    present at initialization. If only domain is supplied -- should be seen as
#'    default -- than the robots.txt file for that domain will be downloaded. If
#'    \code{text} is supplied as well, nothing will be downloaded. If only
#'    \code{text} is supplied than domain is set to '???'.
#'  }
#'  \item{\code{
#'    dummy(buuh)
#'  }}{
#'    TBD
#'  }
#' }
#'
#' @examples
#' mytext <- rtext$new("Hallo World")
#' mytext$text
#' mytext$show_text()
#'
#' # dings
#' mytext <- rtext$new(c("Hallo","World"))
#' mytext$text
#' mytext$show_text()
#'
rtext_char_chode1 <-
  R6::R6Class(

    #### class name ==============================================================
    "rtext",

    #### private =================================================================
    private = list(
      text = function(){
        paste0(private$char, collapse = "")
      },
      tokenize = function(){
        self$token <- self$tokenizer(private$text())
      },

      char        = character(0),
      char_data   = data.frame(),
      token       = data.frame(),
      token_data  = data.frame(),

      hashed_all  = character(),
      hashed_text = character(),
      hashed_data = character(),

      hash_text   = function(){
        private$hashed_text <- dp_hash(private$char)
        private$hashed_all  <- dp_hash(list(private$hashed_data, private$hashed_text))
      },

      hash_data  = function(){
        private$hashed_data <- dp_hash(private$char_data)
        private$hashed_all  <- dp_hash(list(private$hashed_data, private$hashed_text))
      },

      hash_all  = function(){
        private$hashed_data <- dp_hash(private$char_data)
        private$hashed_text <- dp_hash(private$char)
        private$hashed_all  <- dp_hash(list(private$hashed_data, private$hashed_text))
      }
    ),


    #### public ==================================================================
    public = list(


      #### puplic data fields ====================================================
      text_file  = NA,
      tokenizer  = NA,
      encoding   = NA,
      sourcetype = NA,
      id         = NULL,
      save_file  = NULL,

      #### startup function ====================================================

      initialize =
        function(
          text        = NULL,
          text_file   = NULL,
          tokenizer   = rtext_tokenizer$words,
          encoding    = "UTF-8",
          id          = NULL,
          tokenize_by = NULL,
          save_file   = NULL
        )
        {

          ##### read in text // set field: sourcetype
          if(is.null(text) & is.null(text_file)){ # nothing at all
            private$char <- ""
            self$sourcetype <- "empty"
          }else if(is.null(text) & !is.null(text_file)){ # read from text_file
            private$char <- text_read(text_file, tokenize = "", encoding = encoding)
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
          Encoding(private$char) <- encoding
          self$encoding <- "UTF-8"

          #### tokenize
          self$tokenizer <- tokenizer
          if( !is.null(tokenize_by) ){
            self$tokenizer <-
              function(x){
                text_tokenize(x, regex = tokenize_by, non_token = TRUE)
              }
          }

          ##### id
          if( is.null(id) ){
            self$id <- dp_hash(self)
          }

          private$hash_all()
        }
      ,

      #### methods ============================================================
      # info
      info = function(){
        res <-
          list(
            text_file  = self$text_file,
            character  = length(private$char),
            token      = dim(self$token),
            encoding   = self$encoding,
            sourcetype = self$sourcetype
          )
        return(res)
      },
      # show text
      text_show = function(length=500, from=NULL, to=NULL, coll=FALSE, wrap=FALSE){
        text_show(x=self$text_get(Inf), length=length, from=from, to=to, coll=coll, wrap=wrap)
      },
      text_get = function(length=100, from=NULL, to=NULL, split=NULL){
        hash <- paste(self$text_hash(), deparse(match.call()))
        if( !(hash %in% ls(dp_storage)) ){
          res <- rtext_get_character(chars=private$char, length=length, from=from, to=to)
          res <- paste0(res, collapse = "")
          Encoding(res) <- self$encoding
          if( !is.null(split) ){
            res <- unlist(strsplit(res, split = split))
            Encoding(res) <- self$encoding
          }
          assign(hash, res, envir = dp_storage)
          return(res)
        }else{
          res           <- get(hash, envir = dp_storage)
          Encoding(res) <- self$encoding
          return(res)
        }
      },
      # char_get
      char_get = function(length=100, from=NULL, to=NULL, raw=FALSE){
        if(raw | identical(length, TRUE) ){
          res <- private$char
          Encoding(res) <- self$encoding
          return(res)
        }
        res <- rtext_get_character(chars=private$char, length=length, from=from, to=to)
        Encoding(res) <- self$encoding
        return(res)
      },
      # char_get_code
      char_get_code = function(from=1, to=Inf){
        iffer <- private$char_data$i >= from & private$char_data$i <= to
        return(
          data.frame(
            char = private$char[private$char_data[iffer, "i"]],
            private$char_data[iffer, ]
          )
        )
      },
      # add
      char_add = function(what=NULL, after=NULL){
        if( is.null(after) ) {
          private$char <- c(private$char, unlist(strsplit(what,"")))
        }else if ( after==0 ) {
          private$char <- c(unlist(strsplit(what,"")), private$char)
        }else{
          index  <- seq_along(private$char)
          part1  <- private$char[index <= after]
          part2  <- private$char[index >  after]
          private$char <- c( part1, unlist(strsplit(what, "")), part2)
        }
        # necessary updates
        private$hash_text()
        # return for piping
        invisible(self)
      },
      # delete
      char_delete = function(n=NULL, from=NULL, to=NULL){
        private$char <- vector_delete(x = private$char, n=n, from=from, to=to)
        # necessary updates
        private$hash_text()
        # return for piping
        invisible(self)
      },
      # replace
      char_replace = function(from=NULL, to=NULL, by=NULL){
        # check input
        stopifnot( !is.null(from), !is.null(to), !is.null(by) )
        # doing-duty-to-do
        index <- seq_along(private$char)
        private$char <-
          c(
            private$char[index < from],
            unlist(strsplit(by, "")),
            private$char[index > to]
          )
        # necessary updates
        private$has_text()
        # return for piping
        invisible(self)
      },
      # code characters
      char_code = function(x=NULL, val=NULL, i=NULL){
        # prepare input
        if( is.null(x) | is.null(val) | is.null(i) ){
          warning("char_code : no sufficient information passed for x, val, i - nothing coded")
          invisible(self)
        }
        if( length(val)==1 ){
          val <- rep(val, length(i))
        }

        # make indecies
        iffer <- i %in% private$char_data$i
        in_char_data     <- i[iffer]
        not_in_char_data <- i[!iffer]

        # code for i already in char_data
        private$char_data[in_char_data, x] <- val[iffer]

        # code for i not already in char_data
        add_df            <- data.frame(i=not_in_char_data)
        add_df[[x]]       <- val[!iffer]
        private$char_data <- rbind_fill(private$char_data, add_df)

        # necessary updates
        private$hash_data()
        # return for piping
        invisible(self)
      },
      # save
      save = function(file=NULL, id=c("self_id", "hash")){
        # gather information
        tb_saved <-
          list(
            id           = self$id,
            char         = private$char,
            char_data    = private$char_data,
            text_file         = self$text_file,
            encoding     = self$encoding,
            save_file    = self$save_file,
            tokenizer    = self$tokenizer,
            sourcetype   = self$sourcetype,
            token        = private$token,
            token_data   = private$token_data,
            session_info = list(
              rtext_version=packageVersion("rtext"),
              r_version=paste(version$major, version$minor, sep="."),
              version=version
            )
          )
        class(tb_saved) <- c("rtext_save","list")
        # handle id option
        if( id[1] == "self_id"){
          id <- self$id
        }else if( id[1] == "hash"){
          id <- self$hash()
        }else{
          id <- id[1]
        }
        id <- paste0("rtext_", id, collapse = "_")
        # handle file option
        if( is.null(self$save_file) & is.null(file) ){
          stop("rtext$save() : Neither file nor save_file given, do not know where to store file.")
        }else if( !is.null(file) ){
          file <- file
        }else if( !is.null(self$save_file) ){
          file <- self$save_file
        }
        # save to file
        assign(id, tb_saved)
        base::save(list = id, file = file)
        # return for piping
        invisible(self)
      },
      # (re)load
      load = function(file=NULL){
        # handle file option
        if( is.null(file) ){
          stop("rtext$load() : file is not given, do not know where to load file from.")
        }else{
          file <- file
        }
        tmp <- load_into(file)[[1]]

        # setting public
        self$id         <- tmp$id
        self$text_file  <- tmp$text_file
        self$tokenizer  <- tmp$tokenizer
        self$encoding   <- tmp$encoding
        self$sourcetype <- tmp$sourcetype
        self$save_file  <- tmp$save_file

        # setting private
        private$char       <- tmp$char
        private$char_data  <- tmp$char_data
        private$token      <- tmp$token
        private$token_data <- tmp$token_data

        # updating rest
        private$hash_all()

        # return for piping
        invisible(self)
      },
      # save_as
      export = function(){
        message("TBD")
      },
      # text_hash
      text_hash = function(){
        private$hashed_text
      },
      # data_hash
      data_hash = function(){
        private$hashed_data
      },
      # hash
      hash = function(){
        private$hashed_all
      }
    )
  )


















