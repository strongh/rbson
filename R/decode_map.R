##' Map element bytes to decoding functions
##'
##' 
##'
##' @param raw a single raw byte
##' @return a function

decode_map <-
  function(raw){
    switch(as.character(raw),
           "01" = decode_float_element,
           "02" = decode_string_element,
           "07" = decode_objectID_element)
  }

