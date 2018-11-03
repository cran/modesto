context("Length of the list that is returned by the Mt() function.")

test_that("test if Mt() returns a list of length 3", {
  expect_length(Mt(matrix(c(0,2,3,0),2,2,byrow=TRUE),t=0.7,epsilon=0.005), 3)
})
