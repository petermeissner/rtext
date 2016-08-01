#' function tokenizing rtext objects
#' @inheritParams stringb::text_tokenize
#' @method text_tokenize rtext
#' @export
text_tokenize.rtext <-   function(
  x,
  regex       = NULL,
  ignore.case = FALSE,
  fixed       = FALSE,
  perl        = FALSE,
  useBytes    = FALSE,
  non_token   = FALSE
){
  x$text_get() %>%
    text_tokenize(
      regex       = regex,
      ignore.case = ignore.case,
      fixed       = fixed,
      perl        = perl,
      useBytes    = useBytes,
      non_token   = non_token
    ) %>%
    dp_arrange("from","to") %>%
    return()
}


