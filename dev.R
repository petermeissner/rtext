library(stringb)
library(rtext)
library(magrittr)

# initialize (with text or file)
quote <-
  rtext$new(
    text = "Outside of a dog, a book is man's best friend. Inside of a dog it's too dark to read."
  )

# add some data
quote$char_data_set("start", 1, TRUE)
quote$char_data_set("end", quote$char_length(), TRUE)

# get the data
quote$char_data_get()

# transform text
quote$char_add("[this is an insertion] ", 47)

# get the data again (see, the data moved along with the text)
quote$text_get()
quote$char_data_get()

# do some convenience coding (via regular expressions)
quote$char_data_set_regex("dog_friend", "dog", "dog")
quote$char_data_set_regex("dog_friend", "friend", "friend")
quote$char_data_get()

# and now some useful aggregation

private <- quote$get("private")




char_data_aggregate_regex =
  function(
    regex       = NULL,
    ignore.case = FALSE,
    fixed       = FALSE,
    perl        = FALSE,
    useBytes    = FALSE,
    non_token   = FALSE,
    join        = c("","full", "left", "right"),
    aggregate_function = NULL,
    ...
  ){
  join <- join[1]
  # tokenize text
    token <- text_tokenize(private$text(), "\\W", non_token = TRUE)
    token$token_i <- seq_dim1(token)

    # tokenize data and aggegation
    token_data <-
      data.frame(token_i=NULL, start=NULL, end=NULL)
    if( !is.null(private$char_data$i) ){
      # datanize tokens
      token_i <-
        which_token(
          private$char_data$i,
          token$from,
          token$to
        )
      # aggregate data
      if( !is.null(aggregate_function) ){
        # user supplied functions and otpions
        token_data <-
          private$char_data[,-1] %>%
          stats::aggregate(by = list( token_i=token_i ), FUN=aggregate_function, ... )
      }else{
        # standard
        token_data <-
          stats::aggregate(
            private$char_data[,-1],
            by = list( token_i=token_i ),
            FUN="modus",
            multimodal=NA,
            warn=FALSE
          )
      }
#      names(private$token_data)[-1] <- names(private$char_data)[-1]
    }
    # join token and data
    if( join=="full" ){
        res <- merge(token, token_data, all = TRUE)
    }else if( join=="left" ){
        res <- merge(token, token_data, all.x = TRUE)
    }else if( join=="right" ){
        res <- merge(token, token_data, all.y = TRUE)
    }else{
        res <- merge(token, token_data)
    }
    # return
    return(res)
  }

char_data_aggregate_regex("\\W")

















