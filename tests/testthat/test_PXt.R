context("Length of the list that is returned by the PXt() function.")

test_that("test if PXt() returns a list of length 3", {
  expect_length(PXt(c(1,0,0),matrix(c(0,2,0,3,0,1,0,6,0),3,3,byrow=TRUE),t=0.5,epsilon=0.01), 3)
})
