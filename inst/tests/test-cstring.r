## test cstring encoding/decoding

context("cstring")

test_that("encoding and decoding are inverses", {
  samp = "iamcstring"
  expect_that(decode_cstring(encode_cstring(samp)),
              equals(samp))  
})
