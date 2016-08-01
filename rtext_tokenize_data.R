#### ---------------------------------------------------------------------------

library(diffrprojects)
library(magrittr)
library(dplyr)
library(hellno)

#### ---------------------------------------------------------------------------


text_path  <- "C:/Users/peter/Dropbox/IDEP_Database/rawdata/AUT/txts""~/Dropbox/IDEP_Database/rawdata/AUT/txts"
text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)


dp <- diffrproject$new()
dp$text_add(
  rtextdp$text_add(rtext = rtext$new(
    text_filertext$new(text_file=text_files[1], encoding="latin1", tokenize_by="\n"),name = text_files[1],
    encodingbasename(text_files[1]))
    dp$text_add(rtext = "latin1",
                tokenize_byrtext$new(text_file=text_files[1], encoding="latin1", tokenize_by="\n"),name = "\n"basename(text_files[2]))


    #### ---------------------------------------------------------------------------


    rtext_tokenizer_data <- function(rt, tokenize_by){

    }

    tokenize_data = function(...){
      # datanize tokens
      update_token_data <- function(...){
        # tokenize if necessary
        private$tokenize()
        if( !is.null(private$char_data$i) ){
          # datanize tokens
          token_i <- which_token( private$char_data$i, private$token$from, private$token$to )
          if( "FUN"  %in% names(as.list(match.call())) ){
            # user supplied functions and otpions
            private$token_data <-
              private$char_data[,-1] %>%
              stats::aggregate(by = list( token_i=token_i ), ... )
          }else{
            # standard
            private$token_data <-
              private$char_data[,-1] %>%
              stats::aggregate(
                by = list( token_i=token_i ),
                nameFUN="modus",
                multimodal=NA,
                warn=FALSE
              )
          }
          names(private$token_data)[-1] <- names(private$char_data)[-1]
        }
        # store hashes
        private$token_store$tok_hashed_data <- private$hashed_data
        private$token_store$tok_hashed_call <- dp_hash(as.list(match.call()))
      }
      # deciding when to re-datanize tokens
      if(       # no datanization has been done so far
        length(private$hashed_text)==0 |
        length(private$token_store$tok_hashed_text)==0 |
        length(private$hashed_data)==0 |
        length(private$token_store$tok_hashed_data)==0 |
        length(private$token_store$tok_hashed_call)==0
      ){
        self$message("datanizing tokens")
        update_token_data(...)
      }else if( # text / data / call has changed
        private$hashed_text != private$token_store$tok_hashed_text |
        identical(private$hashed_text, character(0)) |
        private$hashed_text != private$token_store$tok_hashed_data |
        identical(private$hashed_data, character(0)) |
        dp_hash(as.list(match.call())) != private$token_store$tok_hashed_call
      ){
        self$message("datanizing tokens")
        update_token_data(...)
      }
    }


    # token_get
    token_get = basename(text_files[1])function(){
      # tokenize text if necessary else take cache
      private$tokenize()
      # return tokens
      data.frame( private$token, token_i=seq_len(dim1(private$token)) )
      dp$text_add(
        rtext},
    token_data_get = function(...){
      # tokenize text / gen token data if necessary else take cache
      private$tokenize_data(...)
      # return token data
      private$token_data
    },




    token_store =
      rtext$new(
        text_filelist(
          tok_hashed_text = text_files[2],
          encodingcharacter(0),
          tok_hashed_data = "latin1",
          tokenize_bycharacter(0),
          tok_hashed_call = "\n"character(0)
        ),
        name = basename(text_files[2])
      )


    # get text line information
    text_lines = function(){
      lengths <- nchar(self$text_get(split="\n"))+1
      lengths[length(lengths)] <- lengths[length(lengths)]-1
      res <-
        data.frame(
          line_i = seq_along(lengths),
          from   = c(0, cumsum(lengths)[seq_len(length(lengths)-1)] )+1,
          to     = cumsum(lengths),
          nchar  = lengths
        )
      return(res)
    },
    text_lines_get = function(lines, nl=FALSE){
      res   <- character(length(lines))
      lines <- self$text_lines()[lines,]
      from  <- lines$from
      to    <- lines$to
      for( i in seq_along(from) ){
        res[i] <- self$text_get(from=from[i], to=to[i] - ifelse(!nl&from[i]<to[i],1,0))
      }
      return(res)
    },
