#' Représentation de OLS 2 variables
#'
#' @param X the X-axis coordinate of an individual
#' @param Y the Y-axis coordinate of an individual
#' @param N the number of individual
#' @return A GGPLOT graph explainig OLS
#' @import ggplot2
#' @import latex2exp
#' @import ggforce
#' @import stats
#' @import knitr
#' @export
#' @examples
#' OlsDiag(data1$X,data1$Y1,nrow(data1))
OlsDiag <- function(X, Y, N, with_OLS = TRUE){
  ########################################
  #####Calculs usuels#####################
  ########################################
  var_X = (1/N) * sum((X - mean(X))^2)
  var_Y = (1/N) * sum((Y - mean(Y))^2)
  COV_XY = (1/N) * sum((X - mean(X))*(Y - mean(Y)))
  CC_P = COV_XY/(sqrt(var_X * var_Y))
  beta = COV_XY/var_X
  alpha = mean(Y) - beta * mean(X)

  #######################################
  ###### LATEX EXPRESSION################
  #######################################
  sigma_x = "$\\sigma_x$"
  sigma_y = "$\\sigma_y$"
  rho_sigma = "$\\rho \\sigma_y$"

  #######################################
  ###### COORDONNATE ####################
  #######################################
  point_X = c(-sqrt(var_X), - sqrt(var_X))
  point_Y = c(sqrt(var_Y), sqrt(var_Y))
  origine = c(0, 0)
  point_inf_OLS = c(-sqrt(var_X), 0)
  point_sup_OLS = c(0, CC_P * sqrt(var_Y))

  #######################################
  ###### R² CURVE #######################
  #######################################
  r = runif(1000, 0, sqrt(var_Y))
  r_carre = r^2 / sqrt(var_Y) ## standardisation


  #######################################
  ###### GRAPHE #########################
  #######################################
  plotrect<-  ggplot() +

    ##Carré pour la variance de X
    geom_rect(
      aes(xmin = origine[1], xmax = point_X[1], ymin = origine[2], ymax= point_X[2]),
      alpha=0.5,
      fill="red"
    ) +

    ## Indiquer la longueur
    geom_segment(
      aes(x = point_X[1], y = point_X[2] , xend = origine[1], yend = point_X[1]),
      arrow = arrow (length = unit(0.2,"cm"), ends = "both")
    )+
    annotate(
      "text",
      x = point_Y[1] / 2,
      y = origine[2] - point_Y[2] * 0.04,
      label=TeX(sigma_y, output="character"),
      vjust=0, size = 4, parse = TRUE
    )+

    ## Carré pour la variance de Y
    geom_rect(
      aes(xmin = origine[1], xmax = point_Y[1], ymin = origine[2], ymax= point_Y[2]),
      alpha=0.5,
      fill = "yellow"
    )+

    ## Indiquer la longueur
    geom_segment(
      aes(x = origine[1], y = origine[2] , xend = point_Y[1], yend = origine[2]),
      arrow = arrow (length = unit(0.2,"cm"), ends = "both")
    )+
    annotate(
      "text",
      x = point_X[1] / 2,
      y = point_X[2] * 1.04,
      label=TeX(sigma_x, output="character"),
      hjust=0, size = 4, parse = TRUE
    )+

    ## Le rectangle representant la covariance
    geom_rect(
      aes(xmin = point_inf_OLS[1], xmax = point_sup_OLS[1], ymin = point_inf_OLS[2], ymax= point_sup_OLS[2]),
      alpha= 0.5,
      fill = "blue"
    )

  if(with_OLS == TRUE){
    plotrect <- plotrect +
      ## Droite de regression dans le rectangle
      geom_segment(
        aes(x = point_inf_OLS[1], y = point_inf_OLS[2], xend = point_sup_OLS[1], yend = point_sup_OLS[2]),
        size = 1.2
      )+
      geom_text(
        aes(x= point_inf_OLS[1] / 2, y = point_sup_OLS[2] / 2 * 1.05, label = "Regression Line"),
        angle = 180 * atan(beta) / pi
      )+
      ### Angle de la droite de regression
      geom_arc(
        aes(x0 = point_X[1], y0 = origine[2], r = sqrt(var_X) * 0.2,  end = 1.57 - atan(beta), start = 1.57),
        linetype = 1
      )
  }
  plotrect <- plotrect +
    ### Carre de ρσy
    geom_rect(
      aes(xmin = origine[1], ymin = origine[2], xmax = point_sup_OLS[2], ymax= point_sup_OLS[2]),
      fill="yellow",
      alpha=0.7
    )+

    ## Indiquer la hauteur
    geom_segment(
      aes(x = origine[1], y = origine[2] , xend = origine[1], yend = point_sup_OLS[2]),
      arrow = arrow (length = unit(0.2,"cm"), ends = "last")
    )+

    annotate(
      "text",
      x = origine[1] + point_inf_OLS[1] * 0.15,
      y = point_sup_OLS[2] / 2,
      label=TeX(rho_sigma, output="character"),
      hjust=0, size = 4, parse = TRUE
    )+
    ### Courbe R²
    geom_line(
      aes(x=r, y = r_carre),
      color="red"
    )+

    ###Rencontre entre R² et carrée ρσy
    geom_segment(
      aes(
        x = point_sup_OLS[2],
        y = (point_sup_OLS[2] ^ 2) / sqrt(var_Y),
        xend = point_Y[1] * 1.07,
        yend = (point_sup_OLS[2] ^ 2) / sqrt(var_Y)
      ),
      arrow = arrow (length = unit(0.2,"cm")),
      linetype = "dashed",
      col = 'red'
    )+
    geom_text(
      aes(x = point_Y[1] * 1.07, y = (point_sup_OLS[2] ^ 2) / sqrt(var_Y), label = "R² = ρ²"),
      size = 2,
      col = "red",
      nudge_x = point_Y[1] * 1.07 * 0.03
    )+

    ### Indication de la standardisation de la variance de Y
    geom_segment(
      aes(x = point_Y[1] * 1.07, y = origine[2], xend = point_Y[1] * 1.07, yend = point_Y[2]),
      col = "red"
    )+

    geom_text(
      aes(x = point_Y[1] * 1.07, y = point_Y[2] * 1.04, label="1"),
      col = "red"
    )+
    geom_text(
      aes(x = point_Y[1] * 1.07, y = origine[2] - point_Y[2] * 0.04, label="0"),
      col = "red"
    )+

    ### Params
    theme(title = element_text(size=10, face="bold"), panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(x=" ", y = " ")+
    scale_x_continuous(breaks=NULL)+
    scale_y_continuous(breaks=NULL)


  ####################################
  ######## OUTPUT ####################
  ####################################
  return(plotrect)
}
