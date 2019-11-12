
context('disks')

test_that('partitions works',{
  dsks <- partitions()
  expect_identical(class(dsks),c("tbl_df","tbl","data.frame"))
})