##' Functions for BSON int64 type
##'
##' The BSON int64 corresponds to the R numeric type.
##'
##' @param num a R numeric to convert
##' @param raw a raw vector to convert
##' @param name the name of a int32 BSON element

encode_int64 <-
  function(num){
    numToRaw(num, nBytes = 8)
  }


decode_int64 <-
  function(raw){
    rawToNum(raw, nBytes = 8)
  }


encode_int64_element <-
  function(name, num){
    raw.num <- numToRaw(num, nBytes = 8)
    raw.name <- encode_cstring(name)
    return(c(
             as.raw(18),
             raw.name,
             raw.num
             ))
  }


decode_int64_element <-
  function(raw){
    if(raw[1] == as.raw(18))
      raw <- raw[-1]
    else
      stop("expected raw(16), got ", as.character(raw[1]))
    first.null <- which(raw==as.raw(0))[1]
    name <- decode_cstring(raw[1:first.null])
    num <- list(decode_int64(raw[(first.null+1):length(raw)]))
    names(num)[1] <- name
    
    num
  }
