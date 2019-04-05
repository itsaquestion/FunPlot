#' CRRA函数
#'
#' @param x
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
CRRA = function(x, theta) {
  if (theta == 1) {
    return(log(x))
  }
  (x ^ (1 - theta) - 1) / (1 - theta)
}

#' CRRA函数2，平移CRRA函数，使x = 0时，有y=0。
#'
#' @param x
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
CRRA2 = function(x, theta) {
  CRRA(x + 1, theta)
}