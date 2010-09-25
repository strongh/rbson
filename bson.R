library(pack)

## maps R types to BSON types
type_map = function(key, val){  
  switch(class(val),
         character = encode_string_element(key, val),
         numeric   = encode_int32_element(key, val),
         list      = encode_document_element(key, val),
         NULL      = encode_null_element(key, val))
}


########################
## document (not the element)
########################

## Expects a list
encode_document = function(List){
  if(length(List) > 0){
    res = mapply(type_map, names(List), List)
    ## first row is strings for each key/value pair
    ## second row is bytes for each pair
    rawl = c(res, recursive=TRUE)
    names(rawl) = NULL
    totalSize = length(rawl) + 4 + 1 # for the int32 before and the trailing null
  } else { # an empty document
    totalSize = 4 + 1
    rawl = c()
  }  
  return(c(
           numToRaw(totalSize, nBytes = 4),
           rawl,
           as.raw(00)
           ))
}

decode_document = function(raw){
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
    num = decode_float_element(raw[1:(first.null+8)])
    doc = append(doc, num)
    raw = raw[-c(1:(first.null+8))]
  }
  return(doc)
}


########################
## document element
########################

encode_document_element = function(key, List){
  if(length(List) > 0){
    res = mapply(type_map, names(List), List)
    ## first row is strings for each key/value pair
    ## second row is bytes for each pair

    rawl = c(res, recursive=TRUE)
    names(rawl) = NULL
    totalSize = length(rawl) + 4 + 1 # for the int32 before and the trailing null
  } else {
    totalSize = 4 + 1
    rawl = c()
  }
  return(c(as.raw(03),
           encode_cstring(key),
           numToRaw(totalSize, nBytes = 4),
           rawl,
           as.raw(00)
           ))
}


########################
## strings (not the element)
########################

## as in String
encode_string = function(chars){
  rw = charToRaw(chars)
  msgLen = length(rw) + 1 # add one for the trailing \x00
  len = numToRaw( # calculate the number of bytes 
    msgLen, 
    nBytes = 4) 
  return(c(len, rw, as.raw(00))) # the formatted string  
}

decode_string = function(raw){
  len = decode_int32(raw[1:4])
  if(len != (length(raw)-4)) { # minus 4 bytes for the first int32
    stop("string should have length (with terminating null) ",
         len,
         " but instead has ",
         length(raw)-4)
  } else {
    if(raw[length(raw)] != as.raw(0))
      stop("Last bytes is ",
           as.character(raw[length(raw)]),
           ", but should be null")
  }
  raw = raw[-c(1:4, length(raw))] # everything is OK. strip first 4 and last bytes    
  
  rawToChar(raw)
}

########################
## string elements
########################

## as in the element UTF-8 string
encode_string_element = function(name, val){
  rw_cstr = encode_cstring(name)
  rw_str = encode_string(val)
  all = c(
    as.raw(02),
    rw_cstr,
    rw_str)
  return(all)
}

decode_string_element = function(raw){
  if (raw[1] == as.raw(02))
    raw = raw[-1] # initial bytes as expected, throw away
  else
    stop(match.call()[1],
         " expected 02 but got ",
         as.character(raw[1]))
  first.null = which(raw==as.raw(0))[1] # index of first null byte
  name = decode_cstring(raw[1:first.null])
  string = list(decode_string(raw[(first.null+1):length(raw)]))
  names(string)[1] = name
  return(string)
}

########################
## cstrings
########################

encode_cstring = function(char){
  rw = charToRaw(char)
  return(c(rw, as.raw(00)))
} 

decode_cstring = function(raw){
  chars = rawToChar(raw[-length(raw)]) # strip off the trailing null
  return(chars)
}


########################
## null elements
########################
encode_null_element = function(name, val){ # val is NULL
  return(c(
           charToRaw('\n'), # 0a
           encode_cstring(name)
           ))
}

decode_null_element = function(raw){ # val is NULL  
  l = list(NULL)
  names(l)[1] = decode_cstring(raw[-1])
  
  l
}


########################
## int32
########################

encode_int32 = function(num){
  numToRaw(num, nBytes = 4)
}

decode_int32 = function(raw){
  rawToNum(raw, nBytes = 4)
}

########################
## int32 element 
########################

encode_int32_element = function(name, num){
  raw.num = numToRaw(num, nBytes = 4)
  raw.name = encode_cstring(name)
  return(c(
           as.raw(16),
           raw.name,
           raw.num
           ))
}

decode_int32_element = function(raw){
  if(raw[1] == as.raw(16))
    raw = raw[-1]
  else
    stop("expected raw(16), got ", as.character(raw[1]))
  first.null = which(raw==as.raw(0))[1]
  name = decode_cstring(raw[1:first.null])
  num = list(decode_int32(raw[(first.null+1):length(raw)]))
  names(num)[1] = name
  
  num
}


########################
## float element 
########################

decode_float_element = function(raw){
  if(raw[1] == as.raw(1))
    raw = raw[-1]
  else
    stop("expected as.raw(1), got ", as.character(raw[1]))
  first.null = which(raw==as.raw(0))[1]
  name = decode_cstring(raw[1:first.null])
  num = unpack("d", raw[(first.null+1):length(raw)])
  names(num)[1] = name
  
  num
}
