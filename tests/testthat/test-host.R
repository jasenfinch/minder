test_that("hostname does not throw an error", {
  expect_type(hostname(),'character')
})

test_that('ipaddress does not throw an error',{
  expect_type(ipaddress(),'character')
})
