.ls_pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .ls_pkg_env$config <- NULL
  invisible(NULL)
}
