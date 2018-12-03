context("Test class functions")

test_that("Test that class functions work as expected", {
  device1 <- device()
  num_vec <- 1:5
  char_vec <- LETTERS
  roll100 <- roll(device1, 100)

  expect_true(is.device(device1))
  expect_false(is.device(num_vec))
  expect_false(is.device(char_vec))

  expect_true(is.rolls(roll100))
  expect_false(is.rolls(num_vec))
  expect_false(is.rolls(char_vec))

})

context("Test that functions are creating the proper classes")

test_that("Test functions are turning out the proper classes", {
  device1 <- device()
  num_vec <- 1:5
  char_vec <- LETTERS
  roll100 <- roll(device1, 100)

  expect_is(roll100, "rolls")
  expect_is(device1, "device")
  expect_is(summary(roll100), "summary.rolls")
})

context("Test that the functions are working properly")

test_that("Test functions are throwing errors when they should", {

  expect_error(device(sides = c("side1", "side1")), "\n'sides' cannot have duplicated elements")
  expect_error(device(prob = c(1, 2, 3)), "\nSides must have same length as probabilities")
  expect_error(device(prob = c(1)), "\nSides must have same length as probabilities")
  expect_error(device(sides = device1), "unimplemented type 'list' in 'orderVector1'\n")
  expect_error(device(prob = device1),"\n'prob' must a vector of numbers between 0 and 1")
  expect_error(device(sides = 1), "\nSides must have same length as probabilities")

})

context("Testing for proper methods")

test_that("Test that implemented methods are working properly", {
  device1 <- device()
  roll100 <- roll(device1, 100)
  roll200 <- roll100 + 100

  expect_length(roll200$rolls, 200)
  expect_length(roll200, 4)
  expect_length(roll100, 4)
  expect_length(device1, 2)
  expect_length(roll200$total, 1)
})

context("Test if function check_sides throws erros when expected")

test_that("check_sides fails with invalid lengths", {

  expect_error(check_sides(c('one')))
  expect_error(check_sides(1))
})

context("Check that probabilities passed to device will work when expected")

test_that("check_prob works with ok vectors", {

  expect_true(check_prob(c(0.5, 0.5)))
  expect_true(check_prob(c(0, 1)))
  expect_true(check_prob(c(1, 0)))
  expect_true(check_prob(c(0.1, 0.9)))
  expect_true(check_prob(c(1/3, 2/3)))
  expect_true(check_prob(c(1/6, 5/6)))
})

context("Check that probabilites passed to device will fail when expected")

test_that("check_prob fails with invalid lengths", {

  expect_error(check_prob(1:5))
  expect_error(check_prob(1))
})


test_that("check_prob fails with invalid numbers", {

  expect_error(check_prob(0.333, 0.666))
  expect_error(check_prob(-0.5, 0.5))
  expect_error(check_prob(0.5, -0.5))
  expect_error(check_prob(0.5, NA))
})
