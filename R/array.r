##' Deserialize embedded array
##'
##'
##' @param raw a raw vector
##' @return a named list whose single element is a list

encode_array_element <-
  function(key, List){
    if(length(List) > 0){
      res = mapply(type_map, as.character(1:length(List)), List)
      ## first row is strings for each key/value pair
      ## second row is bytes for each pair
      
      rawl = c(res, recursive=TRUE)
      names(rawl) = NULL
      totalSize = length(rawl) + 4 + 1 # for the int32 before and the trailing null
    } else {
      totalSize = 4 + 1
      rawl = c()
    }
    return(c(as.raw(04),
             encode_cstring(key),
             numToRaw(totalSize, nBytes = 4),
             rawl,
             as.raw(00)
             ))
  }


##' Deserialize embedded array
##'
##'
##' @param raw a raw vector
##' @return a named list whose single element is a list

decode_array_element <-
  function(raw){
    if(raw[1] == as.raw(04))
      raw = raw[-1]
    else
      stop("expected raw(04), got ", as.character(raw[1]))

    first.null = which(raw==as.raw(0))[1]
    name = decode_cstring(raw[1:first.null])
    doc = unlist(decode_document(raw[(first.null+1):(length(raw))]))
    names(doc) = NULL # otherwise is named vector with integer names.
    doc = list(doc)
    names(doc) = name
    
    doc
  }
