##' Serialize null elements
##'
##' The natural R type to the BSON Null value is NULL.
##'
##' BSON format:
##' 0A e_name
##' 
##' @param name a char from the R names, to be used as the BSON e_name
##' @param val should be NULL
##' @return a raw vector

encode_null_element <-
  function(name, val){ 
    return(c(
             charToRaw('\n'), # 0a
             encode_cstring(name)
             ))
  }

##' Deserialize null elements
##'
##' The natural R type to the BSON Null value is NULL.
##' The raw vector should begin with 0A, marking a BSON null.
##'
##' BSON format:
##' 0A e_name
##'
##' @param raw a raw vector
##' @return a named list whose single element is NULL

decode_null_element <-
  function(raw){ # val is NULL  
    l <- list(NULL)
    names(l)[1] <- decode_cstring(raw[-1])
    
    l
  }
