##' Functions for BSON boolean type
##'
##' The BSON boolean corresponds to the R numeric type.
##'
##' @param num a R boolean to convert
##' @param raw a raw vector to convert
##' @param name the name of a boolean BSON element

encode_logical <-
  function(bool){
    if(bool)
      as.raw(01)
    else
      as.raw(00)
  }


decode_logical <-
  function(raw){
    if(raw == as.raw(01))
      TRUE
    else
      FALSE
  }


encode_logical_element <-
  function(name, bool){
    raw.bool = encode_logical(bool)
    raw.name = encode_cstring(name)
    return(c(
             as.raw(08),
             raw.name,
             raw.bool
             ))
  }


decode_logical_element <-
  function(raw){
    if(raw[1] == as.raw(08))
      raw = raw[-1]
    else
      stop("expected raw(08), got ", as.character(raw[1]))
    first.null = which(raw==as.raw(0))[1]
    name = decode_cstring(raw[1:first.null])
    num = list(decode_logical(raw[(first.null+1):length(raw)]))
    names(num)[1] = name
    
    num
  }
