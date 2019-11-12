
context('memory')

test_that('memoryInfo works',{
  memInf <- memoryInfo()
  expect_identical(class(memInf),c("tbl_df","tbl","data.frame"))
})

test_that('convertUnits works',{
  a <- convertUnits(1024,'MB','GB')
  expect_equal(a,1)
})

test_that('memoryAvailable works',{
  memAvail <- memoryAvailable()
  expect_type(memAvail,'double')
})

test_that('memoryTotal works',{
  memTot <- memoryTotal()
  expect_type(memTot,'double') 
})

test_that('memoryUsed works',{
  memUsed <- memoryUsed()
  expect_type(memUsed,'double')
})

test_that('memoryUser works',{
  memUser <- memoryUser()
  expect_identical(class(memUser),c("tbl_df","tbl","data.frame"))
})