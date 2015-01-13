#' Check if list is empty
#'
#' @param x a list
#' @return TRUE when a is empty (length 0)

is_empty <- function(x) {length(x) == 0}

#' Check if all list vectors have equal length
#' @param x list of vectors
#' @return logical

equal_length <- function(x) {
  x.length <- sapply(x, length)
  length(unique(x.length)) == 1
}
