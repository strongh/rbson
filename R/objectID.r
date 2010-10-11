decode_objectID_element <-
  function(raw){
    if(raw[1] == as.raw(7))
      raw <- raw[-1]
    else
    stop("expected as.raw(7), got ", as.character(raw[1]))
    first.null <- which(raw==as.raw(0))[1]
    name <- decode_cstring(raw[1:first.null])
    num <- rawToNum(raw[(first.null+1):length(raw)], nBytes = 12)
    names(num)[1] <- name
    
    num  
  }
