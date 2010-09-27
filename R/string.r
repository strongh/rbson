encode_string <-
  function(chars){
    rw = charToRaw(chars)
    msgLen = length(rw) + 1 # add one for the trailing \x00
    len = numToRaw( # calculate the number of bytes 
      msgLen, 
      nBytes = 4) 
    return(c(len, rw, as.raw(00))) # the formatted string  
  }


encode_string_element <-
  function(name, val){
    rw_cstr = encode_cstring(name)
    rw_str = encode_string(val)
    all = c(
      as.raw(02),
      rw_cstr,
      rw_str)
  return(all)
  }

decode_string <-
  function(raw){
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

decode_string_element <-
  function(raw){
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
