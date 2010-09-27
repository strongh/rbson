##' Serialize cstring elements
##'
##' Converts between R chars and BSON cstrings.
##' cstrings are typically used as e_names.
##' 
##' @param name a char from the R names, to be used as the BSON e_name
##' @param val should be NULL
##' @return a raw vector

encode_cstring <-
  function(char){
    rw = charToRaw(char)
    return(c(rw, as.raw(00)))
  }

##' Deserialize null elements
##'
##' The natural R type to the BSON Null value is NULL.
##'
##' @param raw a raw vector
##' @return a named list whose single element is a char

decode_cstring <-
  function(raw){
    chars = rawToChar(raw[-length(raw)]) # strip off the trailing null
    return(chars)
  }

