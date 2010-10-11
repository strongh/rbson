## test string encoding/decoding

context("array")

test_that("encoding and decoding are inverses", {
  samp <- 2:40
  expect_that(decode_array_element(encode_array_element("anarray", samp))[[1]],
              equals(samp))
})

