#' @title Print an oneway object
#' @description Prints the results of an oneway ANOVA
#' @param x an object of class oneway
#' @param ... parameters passed to print function
#' @return NULL
#' @examples
#' mileage <- oneway(hwy ~ class, cars)
#' print(mileage)
#' @rdname print.oneway
#' @export

print.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  cat("\nSummary Statistics\n",
      "====================================================\n", sep="")
  print(x$summarystats, ...)
  cat("\nAnova\n",
      "====================================================\n", sep="")
  print(summary.lm(x$anova))
}
