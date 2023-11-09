test_that("ScatterLinePlot function tests", {

  # Test 1: Vector with no NA's
  data1 <- data.frame(
    Group = c("A", "A", "B", "B", "C", "C"),
    X = c(1, 2, 3, 4, 5, 6),
    Y = c(7, 8, 9, 10, 11, 12)
  )
  plot1 <- ScatterLinePlot(data1, Group, X, Y)
  expect_true(!is.null(plot1), info = "Test 1 failed.")

  # Test 2: Vector with NA's
  data2 <- data.frame(
    Group = c("A", "A", "B", "B", "C", "C"),
    X = c(1, 2, 3, NA, 5, 6),
    Y = c(7, 8, 9, 10, 11, NA)
  )
  plot2 <- ScatterLinePlot(data2, Group, X, Y)
  expect_true(!is.null(plot2), info = "Test 2 failed.")

  # Test 3: Vector of a different type
  data3 <- data.frame(
    Group = c("A", "A", "B", "B", "C", "C"),
    X = c(1, 2, 3, 4, 5, 6),
    Y = c("A", "B", "C", "D", "E", "F")
  )
  expect_error(ScatterLinePlot(data3, Group, X, Y), "y_var must be a numeric variable.", info = "Test 3 failed.")

  # Test 4: Vector of length 0
  data4 <- data.frame(
    Group = character(0),
    X = numeric(0),
    Y = numeric(0)
  )
  plot4 <- ScatterLinePlot(data4, Group, X, Y)
  expect_true(!is.null(plot4), info = "Test 4 failed.")

})
