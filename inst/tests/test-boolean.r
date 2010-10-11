context("boolean")

test_that("encoding and decoding are inverses", {
  samp <- TRUE
  expect_that(decode_logical(encode_logical(samp)),
              equals(samp))  
})


test_that("length is correctly determined", {
  samp <- FALSE
  raws <- encode_logical(samp)
  
  expect_that(length_map(c(as.raw(08), raws[1:4])),
              equals(length(raws)))
})
