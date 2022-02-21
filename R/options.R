#' Options class
#'
#' @noRd
#' @description Some Description
#' @importFrom R6 R6Class
#'
Options <- R6::R6Class(
    "Options",
    private = list(
        .sphericity        = FALSE,
        .compound_symmetry = FALSE,
        .multiv_tests      = NULL,
        .univ_tests        = NULL,
        .randomization     = list(ncores = 1, nsamples = 1e3)
    ),
    active = list(
        #' @field sphericity Sphericity option.
        sphericity = function(value) {
            if (missing(value)) {
                private$.sphericity
            } else {
                private$.sphericity <- value
                self
            }
        },

        #' @field compound_symmetry Compound symmetry option.
        compound_symmetry = function(value) {
            if (missing(value)) {
                private$.compound_symmetry
            } else {
                private$.compound_symmetry <- value
                self
            }
        },

        #' @field multiv_tests Multivariate tests option.
        multiv_tests = function(value) {
            if (missing(value)) {
                private$.multiv_tests
            } else {
                private$.multiv_tests <- value
                self
            }
        },

        #' @field univ_tests Univariate tests option.
        univ_tests = function(value) {
            if (missing(value)) {
                private$.univ_tests
            } else {
                private$.univ_tests <- value
                self
            }
        },

        #' @field randomization Randamization tests option.
        randomization = function(value) {
            if (missing(value)) {
                private$.randomization
            } else {
                private$.randomization <- value
                self
            }
        }
    )
)




