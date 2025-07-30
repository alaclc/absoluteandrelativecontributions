#' @title Absolute and Relative Contributions
#' @description It calculates the absolute and relative contributions based on the loading matrix of a Principal Component Analysis
#' @author Alexis Junior La Cruz
#' @param Loadings A numeric matrix of PCA loadings.
#' @return This function prints the absolute and relative contributions for each component.
#' @export
#' @examples
#' loadings <- as.data.frame(pca$loadings[, 1:2])
#'
#' ARC(loadings)
ARC <- function(Loadings){
  cuadrado <- data.frame(Loadings^2)
  sumatoria_cuadrado <- colSums(cuadrado)
  resultado <- sweep(cuadrado, 2, sumatoria_cuadrado, FUN = "/")*100
  cat("Absolute Contributions:\n")
  print(resultado)
  dist <- data.frame(rowSums(cuadrado))
  resultado2 <- sweep(cuadrado, 1, dist$rowSums.cuadrado., "/")*100
  cat("Relative Contributions:\n")
  print(resultado2)
}
