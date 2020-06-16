#' Transformations of factors to logical variables
#'
#' @param X is the covariate matrix
#'
#' @return returns a numeric-only covariate matrix
#' @export
#' @examples
#' toNumericTable()
#'

toNumericTable=function(X) {
  require(data.table)
  d=data.table(X)
  for( col in colnames(d) ) {
    if( is.factor(d[[col]]) ) {
      d[, c(paste0(col,".",levels(d[[col]]))[-1], col) :=
          c(lapply(levels(d[[col]])[-1], function(x) as.integer(x == d[[col]])), .(NULL))]
    }
  }
  return(as.data.frame(d))
}
