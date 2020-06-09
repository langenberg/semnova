#' @keywords internal
init_options <- function() {
    opts <- list(
        sphercitiy        = FALSE,
        compound_symmetry = FALSE,
        verbose           = FALSE,
        multiv_tests      = NULL,
        univ_tests        = NULL,
        randomization     = list(ncores = 1, nsamples = 1e3)
    )
    return(opts)
}

#' @keywords internal
set_options <- function(mod,
                        sphericity        = NULL,
                        compound_symmetry = NULL,
                        verbose           = NULL,
                        multiv_tests      = NULL,
                        univ_tests        = NULL,
                        randomization     = NULL) {

    if (!is.null(sphericity)) {
        mod@opts$sphericity <- sphericity
    }
    if (!is.null(compound_symmetry)) {
        mod@opts$compound_symmetry <- compound_symmetry
    }
    if (!is.null(verbose)) {
        mod@opts$verbose <- verbose
    }
    if (!is.null(multiv_tests)) {
        multiv_tests <- tolower(multiv_tests)
        if (length(indices <- which(!(multiv_tests %in% c("wilks", "wald", "randomization")))) > 0) {
            stop(paste0("I don't know these multivariate tests:\n",
                        multiv_tests[indices]))
        } else {
            mod@opts$multiv_tests <- multiv_tests
        }
    }
    if (!is.null(univ_tests)) {
        univ_tests <- tolower(univ_tests)
        if (length(indices <- which(!(univ_tests %in% c("f", "randomization")))) > 0) {
            stop(paste0("I don't know these multivariate tests:\n",
                        univ_tests[indices]))
        } else {
            mod@opts$univ_tests <- univ_tests
        }
    }
    if (!is.null(randomization)) {
        mod@opts$randomization <- randomization
    }
    if (!is.null(verbose)) {
        mod@opts$verbose <- verbose
    }
    return(mod)
}

#' @keywords internal
get_option <- function(mod, opt) {
    return(mod@opts[[opt]])
}
