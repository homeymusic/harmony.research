test_that('relative periodicity matches the Stolzenburg paper',{
  expect_equal(relative_periodicity(c(0,4,7),'tonic'),4)
  expect_equal(relative_periodicity(c(7,0,4),'tonic'),4)
  expect_equal(relative_periodicity(c(0,2),'tonic'),8)
  expect_equal(relative_periodicity(c(0,4),'tonic'),4)
  expect_equal(relative_periodicity(c(0,6),'tonic'),5)
  expect_equal(relative_periodicity(c(0,3,9),'tonic'),15)
  expect_equal(relative_periodicity(c(0,3,9)-3,'tonic'),25)
  expect_equal(relative_periodicity(c(0,3,9)-9,'tonic'),6)
  expect_equal(relative_periodicity(c(0,16,19),'tonic'),2)
  expect_equal(relative_periodicity(c(0,3,7),'tonic'),10)
  expect_equal(relative_periodicity(0:11,'tonic'),360)
})

# using values from http://artint.hs-harz.de/fstolzenburg/harmony/table15.xls
# we use d=0.0102 which means our tritone matches rational tuning #2
# and our minor 7th matches rational tuning #1
test_that('smoothed relative periodicity matches the Stolzenburg paper',{
  expect_equal(smoothed_relative_periodicity(c(0,0),'tonic'),0)
  expect_equal(smoothed_relative_periodicity(c(0,7),'tonic'),1)
  expect_equal(smoothed_relative_periodicity(c(0,9),'tonic'),1.585,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,5),'tonic'),1.585,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,4),'tonic'),2.000,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,3),'tonic'),2.322,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,8),'tonic'),2.322,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,11),'tonic'),3.000,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,2),'tonic'),3.000,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,2),'tonic'),3.000,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,10),'tonic'),3.170,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,6),'tonic'),2.565,tolerance = 0.001)
  expect_equal(smoothed_relative_periodicity(c(0,1),'tonic'),3.907,tolerance = 0.001)
})
