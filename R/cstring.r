encode_cstring <-
  function(char){
    rw = charToRaw(char)
    return(c(rw, as.raw(00)))
  }

decode_cstring <-
  function(raw){
    chars = rawToChar(raw[-length(raw)]) # strip off the trailing null
    return(chars)
  }

