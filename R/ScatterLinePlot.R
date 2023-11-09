#' Create a Scatterplot with each points connected with grouping of a specific variable
#'
#' This function creates a scatterplot with each of the points connected by a line, and is grouped based on a specified variable. It also filters out any NA values in the y variable.
#'
#' @param data A data frame containing the dataset to be plotted. Named "data" as it is the data frame that you are using to create the plot.
#' @param group_var A character variable used for grouping the data. Named "group_var" as it is the variable that you are grouping by.
#' @param x_var A numeric variable for the x-axis of the scatterplot. Named "x_var" as it is the variable that goes on the x-axis.
#' @param y_var A numeric variable for the y-axis of the scatterplot. Named "y_var" as it is the variable that goes on the y-axis.
#'
#' @return A scatterplot with each points connected by a line, grouped based on the specified variable.
#'
#' @examples
#' # Load required packages (if not already loaded)
#' library(ggplot2)
#' library(dplyr)
#' library(ScatterLinePlot)
#'
#' # Create a sample data frame
#' df <- data.frame(
#'   Group_var = rep(letters[1:3], each = 4),
#'   X_var = rep(1:4, times = 3),
#'   Y_var = c(3, 5, 2, 6, 4, 7, NA, 8, 1, 3, 9, 4)
#' )
#'
#' # Create a scatterplot with lines connecting points
#' ScatterLinePlot(df, Group_var, X_var, Y_var)
#'
#' @export
ScatterLinePlot <- function(data, group_var, x_var, y_var) {

  # Check if group_var is character
  if (!is.character(data %>% dplyr::pull({{group_var}}))) {
    stop("group_var must be a character variable.")
  }

  # Check if x_var is numeric
  if (!is.numeric(data%>% dplyr::pull({{x_var}}))) {
    stop("x_var must be a numeric variable.")
  }

  # Check if y_var is numeric
  if (!is.numeric(data %>% dplyr::pull({{y_var}}))) {
    stop("y_var must be a numeric variable.")
  }

  # Groups data by a variable
  data_grouped <- data %>%
    dplyr::group_by({{group_var}})

  # Filters out any NA values
  data_filtered <- data_grouped %>%
    dplyr::filter(!is.na({{y_var}}))

  # Plots graph
  ggplot2::ggplot(data_filtered, ggplot2::aes(x = {{x_var}}, y = {{y_var}}, color = {{group_var}})) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw()
}
