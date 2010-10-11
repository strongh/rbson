##' Encode a BSON document
##'
##' Translates R list into a BSON document
##'
##' @export
##' @param List a list to encode

encode_document <-
  function(List){
    if(length(List) > 0){
      res <- mapply(type_map, names(List), List)
      ## first row is strings for each key/value pair
      ## second row is bytes for each pair
      rawl <- c(res, recursive=TRUE)
      names(rawl) <- NULL
      totalSize <- length(rawl) + 4 + 1 # for the int32 before and the trailing null
    } else { # an empty document
      totalSize <- 4 + 1
      rawl <- c()
    }  
    return(c(
             numToRaw(totalSize, nBytes = 4),
             rawl,
             as.raw(00)
             ))
  }

