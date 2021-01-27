#' R6 class - linking text and data
#'
#' @docType class
#' @name rtext_tokenize
#'
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @seealso \code{\link{rtext}}
#'
rtext_tokenize <-
  R6::R6Class(

    #### misc ====================================================================
    classname    = "rtext_tokenize",
    active       = NULL,
    inherit      = rtext_export,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('rtext'),



    #### private =================================================================
    private = list(),



    #### public ==================================================================
    public = list(

      #### [ tokenize_data_regex ] #### ..........................................
      tokenize_data_regex =
        function(
          split       = NULL,
          ignore.case = FALSE,
          fixed       = FALSE,
          perl        = FALSE,
          useBytes    = FALSE,
          non_token   = FALSE,
          join        = c("full", "left", "right", ""),
          aggregate_function = NULL,
          ...
        ){
          # tokenize text
          token <-
            text_tokenize(
              private$text(),
              regex       = split,
              ignore.case = ignore.case,
              fixed       = fixed,
              perl        = perl,
              useBytes    = useBytes,
              non_token   = non_token
            )

          # tokenize data and aggegation
          self$tokenize_data_sequences(token=token, join=join, aggregate_function=aggregate_function, ...)
        },

      #### [ tokenize_data_sequences ] #### ....................................
      tokenize_data_sequences = function(
        token,
        join               = c("full", "left", "right", ""),
        aggregate_function = NULL,
        ...
      ){
        token$token_i <- seq_dim1(token)

        join <- ifelse(is.numeric(join), c("full", "left", "right", "")[join], join[1])

        # tokenize data and aggegation
        token_data <-
          data.frame(token_i=NULL, start=NULL, end=NULL)
        chardata <- self$char_data_get()

        if( !is.null( chardata$i) ){
          # datanize tokens
          token_i <-
            which_token(
              chardata$i,
              token$from,
              token$to
            )
          # aggregate data
          if( !is.null(aggregate_function) ){
            # user supplied functions and otpions
            token_data <-
              chardata[,-c(1,2)] %>%
              stats::aggregate(by = list( token_i=token_i ), FUN=aggregate_function, ... )
          }else{
            # standard
            token_data <-
              stats::aggregate(
                chardata[,-c(1:2)],
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
      },


      #### [ tokenize_data_words ] #### ........................................
      tokenize_data_words =
        function(
          split       = "\\W+",
          ignore.case = FALSE,
          fixed       = FALSE,
          perl        = FALSE,
          useBytes    = FALSE,
          non_token   = FALSE,
          join        = c("full", "left", "right", ""),
          aggregate_function = NULL,
          ...
        ){
          self$tokenize_data_regex(
            split       = split,
            ignore.case = ignore.case,
            fixed       = fixed,
            perl        = perl,
            useBytes    = useBytes,
            non_token   = non_token,
            join        = join,
            aggregate_function = aggregate_function,
            ...
          )
        },

      #### [ tokenize_data_lines ] #### ..........................................
      tokenize_data_lines =
        function(
          split       = "\n",
          ignore.case = FALSE,
          fixed       = FALSE,
          perl        = FALSE,
          useBytes    = FALSE,
          non_token   = FALSE,
          join        = c("full", "left", "right", ""),
          aggregate_function = NULL,
          ...
        ){
          self$tokenize_data_regex(
            split       = split,
            ignore.case = ignore.case,
            fixed       = fixed,
            perl        = perl,
            useBytes    = useBytes,
            non_token   = non_token,
            join        = join,
            aggregate_function = aggregate_function,
            ...
          )
        }
    )
  )












