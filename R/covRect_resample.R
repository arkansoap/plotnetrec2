#' Représentation de la covariance après réchantillonage
#'
#' @param data a dataset
#' @param size size of the dataset after resampling
#' @return A mist of GGPLOT graph following resampling
#' @import ggplot2
#' @export
#' @examples
#' covRect_resample(data1)[[1]]

covRect_resample <- function(datas, size) {
  splitter <- datas
  data_resample <- sample_n(splitter, size= size, replace = F)
  graphs_sample <- covRect(data_resample)
  return(graphs_sample)
}
