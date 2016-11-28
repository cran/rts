.onAttach <- function(libname, pkgname) {
  pkg.info <- utils::packageDescription('rts') 
  packageStartupMessage(paste("rts ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))
  .setAuth()
  invisible(0)
}