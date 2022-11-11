test_that("when's July 4th", {

  expect_message(
    whensJuly4()
  )

  expect_message(
    whensJuly4("2022-01-01")
  )

  expect_message(
    whensJuly4("2022-07-04")
  )

  expect_message(
    whensJuly4("2022-07-05")
  )

  expect_error(
    whensJuly4("not a date")
  )

})
