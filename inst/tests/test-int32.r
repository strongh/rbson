context("int32")

test_that("encoding and decoding are inverses", {
  samp <- 2^10
  expect_that(decode_int32(encode_int32(samp)),
              equals(samp))  
})


test_that("length is correctly determined", {
  samp <- 142
  raws <- encode_int32(samp)
  
  expect_that(length_map(c(as.raw(16), raws[1:4])),
              equals(length(raws)))
})
