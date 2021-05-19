#' @export
.onUnload <- function (libpath) {
  library.dynam.unload("mypackage", libpath)
}
# following https://r-pkgs.org/src.html?q=rcpp#cpp-export
# not sure if it needs to be here.
