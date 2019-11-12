
context('cpu')

test_that('cpuInfo works',{
  a <- cpuInfo()
  expect_identical(class(a),c("tbl_df","tbl","data.frame"))
})

test_that('cpus works',{
  a <- cpus()
  expect_type(a,'double')
})

test_that('cpuLoad works',{
  a <- cpuLoad()
  expect_type(a,'double')
})

test_that('cpuUser works',{
  a <- cpuUser()
  expect_identical(class(a),c("tbl_df","tbl","data.frame"))
})