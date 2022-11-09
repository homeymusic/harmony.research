test_that("rotation works", {
  angle = pi/4
  expect_equal(rotate(cbind(x=1,y=0),angle),cbind(0.5,0.5))
  expect_equal(rotate(cbind(x=0,y=1),angle),cbind(-0.5,0.5))
})
test_that('chord combos are solid',{
  expect_equal(nrow(chord_combinations(list(c(0,4,7),c(0,3,7)))),12)
})
