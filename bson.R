## maps R types to BSON types
type_map = function(key, val){  
  switch(class(val),
         character = encode_string_element(key, val))
}

## Expects a list
encode_document = function(List){
  res = mapply(type_map, names(List), List)
  ## first row is strings for each key/value pair
  ## second row is bytes for each pair
  str = Reduce(paste, res[1,])
  totalSize = Reduce(sum, res[2,]) + 4 + 1 # for the int32 before and the trailing null
  return(paste(
               formatRaw(numToRaw(totalSize, nBytes = 4)),
               str,
               "\\x00",
               sep = ""
               ))
}

## formats bytes
formatRaw = function(raws){
  paste(paste("\\x", raws, sep=""), collapse="")
}



## as in String
encode_string = function(chars){
  rw = charToRaw(chars)
  msgLen = length(rw) + 1 # add one for the trailing \x00
  len = numToRaw( # calculate the number of bytes 
    msgLen, 
    nBytes = 4) 
  formatted = paste(
    formatRaw(len),
    chars,
    "\\x00",
    sep = "")
  return(list(
              str =     formatted, # the formatted string
              nbytes = msgLen + 4 # add 4 bytes for the preppended int32 
              ))  
}

## as in the element UTF-8 string
encode_string_element = function(name, val){
  cstr = encode_cstring(name)
  str = encode_string(val)
  all = paste(
    "\\x02",
    cstr$str,
    str$str,
    sep = "")
  return(list(
              str = all,
              nbytes = cstr$nbytes + str$nbytes + 1 # 1 for the \x02 opener
              ))
}

encode_cstring = function(char){
  str = paste(char, "\\x00", sep="")
  rw = charToRaw(char)
  msgLen = length(rw) + 1 # add one for the trailing \x00
  return(list(str=str, nbytes=msgLen))
} 
