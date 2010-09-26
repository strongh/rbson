decode_float_element <-
  function(raw){
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
