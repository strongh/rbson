##' Functions for BSON datetime type
##'
##' The BSON datetime is UTC millisecond since the unix epoch. 
##' This is conveniently the internal representation of dates in R.
##' 
##' @param num a R date to convert
##' @param raw a raw vector to convert
##' @param name the name of a datetime BSON element

encode_datetime <-
  function(datetime){ # BSON wants *milliseconds*, R uses seconds
    numToRaw(unclass(datetime)*1000, nBytes = 8) # stored as int64
  }


decode_datetime <-
  function(raw){
    sec = rawToNum(raw, nBytes = 8)/1000
    as.POSIXlt(sec, origin = "1970-01-01")
  }


encode_datetime_element <-
  function(name, datetime){
    raw.dt = numToRaw(unclass(datetime)*1000, nBytes = 8)
    raw.name = encode_cstring(name)
    return(c(
             as.raw(17),
             raw.dt,
             raw.num
             ))
  }


decode_datetime_element <-
  function(raw){
    if(raw[1] == as.raw(17))
      raw = raw[-1]
    else
      stop("expected raw(17), got ", as.character(raw[1]))
    first.null = which(raw==as.raw(0))[1]
    name = decode_cstring(raw[1:first.null])
    num = list(decode_datetime(raw[(first.null+1):length(raw)]))
    names(num)[1] = name
    
    num
  }
