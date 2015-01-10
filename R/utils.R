#' Check if list is empty
#'
#' @param a a list
#' @return TRUE when a is empty (length 0)

is.empty <- function(a) {length(a) == 0}

#' Check if all list vectors  have equal length
#' @param x list of vectors
#' @return logical

equal.length <- function(x) {
  x.length <- sapply(x, length)
  length(unique(x.length)) == 1
}
