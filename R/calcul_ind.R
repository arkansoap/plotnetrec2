#' Tableau des calculs usuels
#'
#' @param x the X-axis coordinate of an individual
#' @param y the Y-axis coordinate of an individual
#' @param N the number of individual
#' @return a table of usual indicators
#' @export
#' @examples
#' calcul_ind(data1$X,data1$Y,nrow(data1))


calcul_ind <- function (X,Y,N) {
  Mean_X <- mean(X)
  Mean_Y <- mean(Y)
  var_X <- (1/N) * sum((X - mean(X))^2)
  var_Y <- (1/N) * sum((Y - mean(Y))^2)
  COV_XY <- (1/N) * sum((X - mean(X))*(Y - mean(Y)))
  CC_P <- COV_XY/(sqrt(var_X * var_Y))
  V_X_Y <- var_X + var_Y - 2 * sqrt(var_X) * sqrt(var_Y)
  names <- c("Mean_x", "Mean_y", "var_x", "var_y", "cov_XY", "cor", "Var_XY")
  values <- c(Mean_X, Mean_Y, var_X, var_Y, COV_XY, CC_P, V_X_Y)
  df <- data.frame(list(names, values))
  colnames(df) <- (c("indic", "value"))
  df <- format(df, scientific = F)
  return(df)
}
