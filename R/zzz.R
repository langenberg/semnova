.onLoad <- function(libname, pkgname) {
    helper <- Helper$new()
    assign("helper", helper, envir = parent.env(environment()))
}
