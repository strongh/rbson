## test document encoding/decoding

context("document")

test_that("encoding and decoding are inverses", {
  samp = list(a="very basic", numb=442, mydate=Sys.time())
  res = decode_document(encode_document(samp))
  
  for(i in 1:length(samp)){
    expect_that(samp[[1]],
                equals(res[[1]]))
  }
  
})
