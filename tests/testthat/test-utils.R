test_that("rotation works", {
  angle = pi/4
  expect_equal(rotate(cbind(x=1,y=0),angle),cbind(0.5,0.5))
  expect_equal(rotate(cbind(x=0,y=1),angle),cbind(-0.5,0.5))
})
