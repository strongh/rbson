type_map <-
  function(key, val){  
    switch(class(val),
           character = encode_string_element(key, val),
           numeric   = encode_int32_element(key, val),
           list      = encode_document_element(key, val),
           NULL      = encode_null_element(key, val))
  }

