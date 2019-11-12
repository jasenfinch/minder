
context('processes')

test_that('processes works',{
  proc <- processes()
  expect_identical(class(proc),c("tbl_df","tbl","data.frame"))
})