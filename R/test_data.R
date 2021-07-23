#' @keywords internal
get_test_data <- function(n = 100L) {
    vcov <- matrix(0.5, nrow = 6, ncol = 6)
    diag(vcov) <- 1
    eta <- MASS::mvrnorm(n, rep(0, 6), vcov)
    eta <- as.data.frame(eta)
    names(eta) <- paste0("var", 1:6)
    return(eta)
}
