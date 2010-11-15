##' Determine element length
##'
##' 
##'
##' @param raw a single raw byte
##' @return a function

length_map <-
  function(raw){ # should be the first byte
    switch(as.character(raw[1]), # plus the first 4 bytes after the c_string
           "01" = 8,
           "02" = decode_int32(raw[2:5]) + 4, # after
           "03" = decode_int32(raw[2:5]), # after
           "04" = decode_int32(raw[2:5]),
           "07" = 12,
           "08" = 1, 
           "09" = 8,
           "0a" = 0,
           "10" = 4,
           "12" = 8,
           stop("Unsupported BSON element type ", raw[1]))
  }

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
           "03" = decode_document_element,
           "04" = decode_array_element,
           "07" = decode_objectID_element,
           "08" = decode_logical_element,
           "09" = decode_datetime_element,
           "0a" = decode_null_element,
           "10" = decode_int32_element,
           "12" = decode_int64_element)
  }


##' Map R classes to encoding functions
##'
##' 
##'
##' @param raw a single raw byte
##' @return a function

type_map <-
  function(key, val){
    if(!is.list(val) && length(val) > 1){ # catch vectors
      return(encode_array_element(key, val))      
    }
    switch(class(val)[1],
           character = encode_string_element(key, val),
           numeric   = encode_int32_element(key, val),
           integer   = encode_int32_element(key, val),
           list      = encode_document_element(key, val),
           POSIXt    = encode_datetime_element(key, val),
           logical   = encode_logical_element(key, val),
           NULL      = encode_null_element(key, val))
  }
