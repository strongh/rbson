## test string encoding/decoding

context("string")

test_that("encoding and decoding are inverses", {
  samp = "iamstring"
  expect_that(decode_string(encode_string(samp)),
              equals(samp))  
})


test_that("length is correctly determined", {
  samp = "iamstringd dfgefe4jf jiohgior hrguirhui"
  raws = encode_string(samp)
  
  expect_that(length_map(c(as.raw(02), raws[1:4])),
              equals(length(raws)))
})
