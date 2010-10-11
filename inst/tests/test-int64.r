context("int64")

test_that("encoding and decoding are inverses", {
  samp <- 2^34
  expect_that(decode_int64(encode_int64(samp)),
              equals(samp))  
})


test_that("length is correctly determined", {
  samp <- 2^41
  raws <- encode_int64(samp)
  
  expect_that(length_map(c(as.raw(18), raws[1:4])),
              equals(length(raws)))
})
