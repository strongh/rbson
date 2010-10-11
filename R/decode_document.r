##' Deserialize document
##'
##'
##' @export
##' @param raw a raw vector
##' @return a named list whose single element is a list

decode_document <-
  function(raw){
    len = decode_int32(raw[1:4])
    if(len != length(raw)) { # 
      stop("string should have length (with terminating null) ",
           len,
           " but instead has ",
           length(raw))
    } else {
      if(raw[length(raw)] != as.raw(0))
        stop("Last bytes is ",
             as.character(raw[length(raw)]),
             ", but should be null")
    }
    raw = raw[-c(1:4)]
    doc = list()
    while(length(raw) > 1){
      element = raw[1] # the bytes representing the element type

      first.null = match(as.raw(0), raw) # signalling the end of the e_name cstring
      to.determine.len = c(1, (first.null+1):(first.null+4))
      len = length_map(raw[to.determine.len]) # get the length of this element
      
      num = decode_map(element)(raw[1:(first.null+len)])
      doc = append(doc, num)
      raw = raw[-c(1:(first.null+len))]
    }
    return(doc)
  }


##' Deserialize embedded document
##'
##'
##' @param raw a raw vector
##' @return a named list whose single element is a list

decode_document_element <-
  function(raw){
    if(raw[1] == as.raw(03))
      raw = raw[-1]
    else
      stop("expected raw(03), got ", as.character(raw[1]))

    first.null = which(raw==as.raw(0))[1]
    name = decode_cstring(raw[1:first.null])
    doc = list(decode_document(raw[(first.null+1):(length(raw))]))

    doc
  }

