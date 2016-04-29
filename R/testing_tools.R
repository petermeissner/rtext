
#' text function: wrapper for system.file() to access test files
#' @export
#' @param x name of the file
tf <- function(x=NULL){
  if(is.numeric(x)){
    return(tf(tf()[(x-1) %% length(tf()) +1 ]))
  }
  if(is.null(x)){
    return(list.files(system.file("testfiles", package = "diffrprojects")))
  }else if(x==""){
    return(list.files(system.file("testfiles", package = "diffrprojects")))
  }else{
    return(system.file(paste("testfiles", x, sep="/"), package = "diffrprojects") )
  }
}
