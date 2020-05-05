
context('whoami')

test_that('whoami works',{
  user <- whoami()
  expect_true(is.character(user))
})