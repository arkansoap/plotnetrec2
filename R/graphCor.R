#' Représentation de corrélation
#'
#' @param x the X-axis coordinate of an individual
#' @param y the Y-axis coordinate of an individual
#' @param N the number of individual
#' @return A GGPLOT graph
#' @import ggplot2
#' @export
#' @examples
#' graph_cor(data1$X,data1$Y,nrow(data1))

graph_cor <- function(X, Y, N){

  var_X = (1/N) * sum((X - mean(X))^2)
  var_Y = (1/N) * sum((Y - mean(Y))^2)
  COV_XY = (1/N) * sum((X - mean(X))*(Y - mean(Y)))
  CC_P = COV_XY/(sqrt(var_X * var_Y))
  V_X_Y = var_X + var_Y - 2 * sqrt(var_X) * sqrt(var_Y)
  ## FULL POSITIV CORR
  plotrect <- ggplot()+
    geom_rect(aes(xmin = - sqrt(var_X), ymin = -sqrt(var_X), xmax = sqrt(var_Y), ymax = sqrt(var_Y)),
              alpha = 0.3,
              fill = "blue"
    )+

    geom_segment(aes(x = 0, y = sqrt(var_Y), xend = -sqrt(var_X), yend = sqrt(var_Y)),
                 arrow = arrow (length = unit(0.2,"cm")),
                 col = 'blue') +
    geom_segment(aes(x = -sqrt(var_X), y = 0, xend = -sqrt(var_X), yend = -sqrt(var_X)),
                 arrow = arrow (length = unit(0.2,"cm")),
                 col = 'blue') +
    geom_segment(aes(x = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)), y = -sqrt(var_X), xend = sqrt(var_Y), yend = -sqrt(var_X)),
                 arrow = arrow (length = unit(0.2,"cm")),
                 col = 'blue') +
    geom_segment(aes(x = sqrt(var_Y), y = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)), xend = sqrt(var_Y), yend = sqrt(var_Y)),
                 arrow = arrow (length = unit(0.2,"cm")),
                 col = 'blue') +

    ## NO CORR

    geom_polygon(
      aes(
        x = c(0, -sqrt(var_X), sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)), sqrt(var_Y)),
        y = c(sqrt(var_Y), 0, - sqrt(var_X), sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)))
      ),
      alpha = 1
    )+


    ## FULL NEGATIVE CORR
    geom_rect(aes(xmin = 0, ymin = 0, xmax = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)), ymax = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y))),
              fill = "red",
              alpha = 0.3
    )+

    geom_segment(aes(x = 0, y = sqrt(var_Y), xend = 0, yend = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y))),
                 arrow = arrow (length = unit(0.2,"cm")),
                 col = 'red') +
    geom_segment(aes(x = -sqrt(var_X), y = 0, xend = 0, yend = 0),
                 arrow = arrow (length = unit(0.2,"cm")),
                 col = 'red') +
    geom_segment(aes(x = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)), y = -sqrt(var_X), xend = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)), yend = 0),
                 arrow = arrow (length = unit(0.2,"cm")),
                 col = 'red') +
    geom_segment(aes(x = sqrt(var_Y), y = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)), xend = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y)), yend = sqrt(var_X + var_Y - 2 * sqrt(var_X * var_Y))),
                 arrow = arrow (length = unit(0.2,"cm")),
                 col = 'red') +


    ### Params
    theme(title = element_text(size=10, face="bold"), panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(x=" ", y = " ")+
    scale_x_continuous(breaks=NULL)+
    scale_y_continuous(breaks=NULL)
  return(plotrect)
}
