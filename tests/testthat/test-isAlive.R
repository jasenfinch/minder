
context('isAlive')

test_that('isAlive works',{
  a <- isAlive('www.google.com')
  expect_true(a)
})