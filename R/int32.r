encode_int32 <-
  function(num){
    numToRaw(num, nBytes = 4)
  }


decode_int32 <-
  function(raw){
    rawToNum(raw, nBytes = 4)
  }


encode_int32_element <-
  function(name, num){
    raw.num = numToRaw(num, nBytes = 4)
    raw.name = encode_cstring(name)
    return(c(
             as.raw(16),
             raw.name,
             raw.num
             ))
  }


decode_int32_element <-
  function(raw){
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
