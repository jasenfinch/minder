
context('isAlive')

test_that('isAlive works',{
  skip_on_ci()
  a <- isAlive('www.google.com')
  expect_true(a)
})