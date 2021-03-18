#' Clean Numbers
#' @param x object to be parse
#' @param digits the digits to which to round, default of \code{2}
#' @param delim the delimiter to use for the big.mark arguments
#'     default of \code{","}
#' @return a number formatted as specigied (e.g. 1000 becomes 1,000) with defaults
#' @examples
#' clean_numbers(10000)
#' @export


clean_numbers <- function(x, digits = 2, delim = ",") {
  old_selection <- getOption("scipen", default = NULL)
  if(old_selection==0){
    options(scipen = 999)
    on.exit({options(scipen = old_selection)})
  }

  if(!is.numeric(x)){
    x
  } else {
      prettyNum(round(x,digits), big.mark=delim)
  }
}

