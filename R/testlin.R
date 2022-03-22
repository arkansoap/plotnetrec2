#' Graphique pour test de linéarité
#'
#' @param data a dataset
#' @return a set GGPLOT graph
#' @export
#' @examples
#' testlin(data1)

testlin <- function (data) {
  x <- data$x
  y <- data$y
  ols<-lm(y~x,data=data)
  Nmat=N*(N-1)/2
  betahat <- coef(ols)[2]
  sigmae <- sigma(ols)
  ######################## cas 1 ################################################
  # On fait le différentiel de x et de la moyenne de x et on précise le signe -
  xdiff <- outer(x, mean(x), "-")
  # On fait le différentiel de y et de la moyenne de y et on précise le signe -
  ydiff <- outer(y, mean(y), "-")
  wi <- xdiff^2
  bi <- ifelse(xdiff==0,
               0,
               ydiff / xdiff)
  biwi <- (wi*bi)/sum(wi)
  # Slopes representation regarding case n°1: $x_i$
  lupbi = betahat * wi / sum(wi) + 1.96 * (x - mean(x)) * sigmae / sum(wi)
  lcenterbi = betahat * wi / sum(wi)
  ldownbi = betahat * wi / sum(wi) - 1.96 * (x - mean(x)) * sigmae / sum(wi)
  # Création du curseur dans le cas 1:
  cursori <- round(bi/N, 4)
  cursor <- betahat/N
  ######################### cas 2 #############################################
  xdiff_2 <- outer(x, x, "-")
  ydiff_2 <- outer(y, y, "-")
  wij <- xdiff_2^2
  bij <- ifelse(xdiff_2 == 0,
                0,
                ydiff_2 / xdiff_2)
  bijwij <- (wij * bij) / sum(wij)
  Vec_bij = as.numeric(na.omit(matrix(t(sqrt(bij)), (N^2), 1)))
  Vec_wij = as.numeric(na.omit(matrix(t(sqrt(wij)), (N^2), 1)))
  Vec_bijwij = as.numeric(na.omit(matrix(t(bijwij), (N^2), 1)))
  Vec_dij = as.numeric(na.omit(matrix(t((abs(xdiff_2))),(N^2),1)))
  df_bij <- data.frame(Vec_bij)
  df_bijwij <- data.frame(Vec_bijwij)
  #Il y a des 0
  df_toto <-data.frame(cbind(Vec_bijwij,Vec_dij))
  df_toto <-df_toto[order(df_toto$Vec_dij),]
  # Slopes representation regarding the distance between $i$ and $j$: Case n°2
  lupbij = betahat * wij / sum(wij) + 1.96 * sqrt(2) * (sqrt(wij)) * sigmae / sum(wij)
  lcenterbij = betahat * wij / sum(wij)
  ldownbij = betahat * wij / sum(wij) - 1.96 * sqrt(2) * (sqrt(wij)) * sigmae / sum(wij)
  cursorij <- round(Vec_bij/N, 4)
  ########################## cas 3 ############################################
  bip<-numeric()
  bipwip<-numeric()
  wip<-numeric()
  for(i in 1:(N))
  {
    bip[i]<- sum(bij[i,]*wij[i,])/sum(wij[i,])
    #  bipwip[i]<- weighted.mean(as.vector(bij[i,]), as.vector(wij[i,]))
    wip[i]<- sum(wij[i,])/(sum(wij[,]))
  }
  bipwip=bip*wip
  #crÃ©er data frame
  df_bip <- data.frame(bip)
  df_bipwip <- data.frame(bipwip)
  # Slopes representation regarding $x_i$ : Case n°3
  lupbip <- betahat*(((x - mean(x))^2) + sqrt((var(x) / (2 * (N - 1) * var(x)))))
  lcenterbip = betahat*wip
  ldownbip <- betahat*(((x - mean(x))^2) - sqrt((var(x) / (2 * (N - 1) * var(x)))))
  #################### graphics #############################################
  # Scatterplot
  scatter <- ggplot(data = mydata, aes(x, y)) +
    geom_point(colour = "orange",
               shape = 16,
               alpha = 0.7) +
    theme_minimal() +
    geom_smooth(method = "lm",
                color = "green",
                se = FALSE) +
    ggtitle("Scatter Plot")
  # graph 2
  graph_biwi <- ggplot(data = mydata, aes(x, biwi)) +
    geom_point(colour = "red",
               shape = 16,
               alpha = 0.5) +
    geom_point(aes(x, lupbi),
               colour = "yellow",
               shape = 16,
               alpha = 0.5) +
    geom_point(aes(x, lcenterbi),
               colour = "green",
               shape = 16,
               size = 0.8,
               alpha = 0.5) +
    geom_point(aes(x, ldownbi),
               colour = "yellow",
               shape = 16,
               alpha = 0.5) +
    annotate(geom = "text",
             x = 4, y = 0.028,
             label = "Beta = 0.025",
             color="orange",
    ) +
    ggtitle("Case n°1") +
    theme_minimal()
  #graph 3
  graphbijwij <-
    ggplot(data = df_bijwij, aes(Vec_wij,Vec_bijwij))+
    geom_point(colour = "red",
               shape = 16,
               alpha = 0.1)+
    geom_point(aes(Vec_wij, lupbij),
               colour = "yellow",
               shape = 16,
               alpha = 0.1)+
    geom_point(aes(Vec_wij, lcenterbij),
               colour = "green",
               shape = 16,
               alpha = 0.1) +
    geom_point(aes(Vec_wij,ldownbij),
               colour = "yellow",
               shape = 16,
               alpha = 0.1) +
    ggtitle("Case n°2") +
    theme_minimal()
  #graph 4
  graphbip <- ggplot(data = mydata, aes(x, bip)) +
    geom_point(colour = "red",
               shape = 16,
               alpha = 0.5)+
    ggtitle("Case n°3") +
    theme_minimal()
  return(scatter + graph_biwi + graphbijwij + graphbip)
}
