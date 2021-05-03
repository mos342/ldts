#' Outer appends files with different columns
#' @name out_merge
#' @param x first dataset
#' @param y second dataset
#' @export
#' @examples
#' df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3)))
#' df2 = data.frame(CustomerId = c(2, 4, 6), State = c(rep("Alabama", 2), rep("Ohio", 1)))
#' rbind_diff_col(df1,df2)
rbind_diff_col <- function(x, y) {

  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))

  x[, c(as.character(y.diff))] <- NA

  y[, c(as.character(x.diff))] <- NA

  return(rbind(x, y))
}
