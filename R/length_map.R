length_map <-
  function(raw){ # should be the first byte
    switch(as.character(raw[1]), # plus the first 4 bytes after the c_string
           "01" = 8,
           "02" = decode_int32(raw[2:5]) + 4, # after
           "07" = 12,
           "10" = 4,
           stop("Unsupported BSON element type ", raw[1]))
  }

