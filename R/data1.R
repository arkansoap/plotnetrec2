#' A sample with randomly generate with strong variance of Y
#'
#' A dataset containing the coordonates of 500 point
#'
#' @format A data frame with 500 rows:
#' \describe{
#'   \item{X}{sort(rnorm(N, 40, 100)) }
#'   \item{Y}{1- 2*X + rnorm(N, 0, 200) # forte variance}
#'   ...
#' }
"data1"
