#' Check if list is empty
#'
#' @param a a list
#' @return TRUE when a is empty (length 0)

is.empty <- function(a) {length(a) == 0}

#' Check if all elements in vector have equal length
#'
#'

equal.length <- function(a) {
  a.length <- sapply(a, length)
  length(unique(a.length)) == 1
}
