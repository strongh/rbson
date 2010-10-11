context("datetime")

test_that("encoding and decoding are inverses", {
  samp <- Sys.time()
  expect_that(unclass(decode_datetime(encode_datetime(samp)))[1],
              equals(unclass(samp)))  
})


test_that("length is correctly determined", {
  samp <- Sys.time()
  raws <- encode_datetime(samp)
  
  expect_that(length_map(c(as.raw(09), raws[1:4])),
              equals(length(raws)))
})
