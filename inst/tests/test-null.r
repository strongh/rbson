context("null")

test_that("encoding and decoding are inverses", {  
  expect_true(is.null(rbson:::decode_null_element(rbson:::encode_null_element("mynull", NULL))$mynull))
})
