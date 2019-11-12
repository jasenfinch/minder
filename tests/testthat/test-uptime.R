
context('uptime')

test_that('uptime works',{
  time <- uptime()
  expect_type(time,'double')
})