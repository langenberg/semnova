#' Hypothesis class
#' @description Some description
#' @export
#' @importFrom R6 R6Class
Hypothesis <- R6::R6Class(
    "Hypothesis",
    private = list(
        L = matrix(),
        M = matrix(),
        P = matrix(),
        description = "",

        #' @description Validates the input for different functions.
        #' @keywords internal
        #' @param lgc Object of class Lgc.
        validate = function(lgc) {

            if (is.null(private$M)) {
                if (length(lgc$expose$etas) == 1L) {
                    private$M <- matrix(1, ncol = 1, nrow = 1)
                } else {
                    stop("M matrix must be provided")
                }
            } else if (nrow(private$M) != length(lgc$expose$etas)) {
                stop("Number of rows of the M matrix must be equal to the number dependent variables")
            }

            if (is.null(private$L)) {
                if (length(lgc$expose$group_labels) == 1L) {
                    private$L <- matrix(1, ncol = 1, nrow = 1)
                } else {
                    stop("L matrix must be provided")
                }
            } else if (ncol(private$L) != length(lgc$expose$group_labels)) {
                stop("Number of columns of the L matrix must be equal to the number of groups")
            }

            if (is.null(private$P)) {
                if (length(lgc$expose$group_labels) == 0L) {
                    private$P <- matrix(1, ncol = 1, nrow = 1)
                } else {
                    stop("P matrix must be provided")
                }
            } else if (ncol(private$P) != (length(lgc$expose$covariates) + 1)) {
                stop("Number of columns of the P matrix must be equal to the number of covariates + 1.")
            }
        }
    ),
    active = list(

        #' @field get_L Return the L matrix of the hypothesis.
        get_L = function() {
            private$L
        },

        #' @field get_M Return the M matrix of the hypothesis.
        get_M = function() {
            private$M
        },

        #' @field get_P Return the P matrix of the hypothesis.
        get_P = function() {
            private$P
        },

        #' @field get_description Return the description of the hypothesis.
        get_description = function() {
            private$description
        },

        #' @field expose Returns the private structure of the object. For debugging purposes only.
        expose = function() {
            private
        }
    ),
    public = list(

        #' @description Hypothesis class constructor.
        #' @export
        #' @param M M matrix. Can be omitted if the number of dependent
        #' variables equals 1.
        #' @param L L matrix. Can be omitted if the number of groups equals 1.
        #' @param P P matrix. Can be omitted if the number of covariates
        #' equals 0.
        #' @param description Character. Description of the hypothesis. Can be
        #' any character.
        initialize = function(M = NULL, L = NULL, P = NULL, description = "") {
            private$M = M
            private$L = L
            private$P = P
            private$description = description
        },

        #' @description Returns the a string for the Wald test.
        #' @export
        #' @param lgc Object of class Lgc.
        get_wald_string = function(lgc) {

            private$validate(lgc)

            # some helper functions

            mx <- function(x,y) {
                if (is.vector(x) && is.vector(y)) {
                    if (length(x) != length(y)) {
                        stop("wrong dimensions")
                    }
                    indices <- x != "0" | y != "0"
                    if (!any(indices)) {
                        return("0")
                    }
                    x <- x[indices]
                    y <- y[indices]
                    paste0("(", x, ")*(", y, ")") %>%
                        paste0(collapse = " + ") %>%
                        sapply(Deriv::Simplify) %>%
                        return()
                } else if (is.matrix(x) && is.vector(y)) {
                    y <- matrix(y, nrow = ncol(x))
                    mx(x, y)
                } else if (is.vector(x) && is.matrix(y)) {
                    x <- matrix(x, ncol = nrow(y))
                    mx(x, y)
                } else if (is.matrix(x) && is.matrix(y)) {
                    apply(y, 2, function(mycol) {
                        apply(x, 1, function(myrow) mx(myrow, mycol))
                    })
                }
            }

            par_labels <- lgc$get_par_labels

            result1 <- array(NA, dim = c(nrow(par_labels), ncol(private$M), dim(par_labels)[3]))

            for (i in 1:dim(result1)[3]) {
                result1[, , i] <-
                    mx(par_labels[, , i], private$M)
            }

            result2 <- array(NA, dim = c(nrow(private$L), dim(result1)[2], dim(par_labels)[3]))

            for (i in 1:dim(result2)[3]) {
                result2[,,i] <- mx(private$L, result1[,,i])
            }

            result3 <- array(NA, dim = c(dim(result2)[1], dim(result2)[2], nrow(private$P)))

            for (i in 1:dim(result3)[1]) {
                for (j in 1:dim(result3)[2]) {
                    for (k in 1:dim(result3)[3]) {
                        result3[i,j,k] <- mx(private$P[k,,drop=F], matrix(result2[i,j,],ncol=1))
                    }
                }
            }

            apply(result3, 3, function(mat) {
                apply(mat, 2, function(vec) {
                    vec %>%
                        sapply(function(x) paste0(x, " == 0")) %>%
                        paste0(collapse = "\n")
                }) %>% paste0(collapse = "\n")
            }) %>% paste0(collapse = "\n")
        }
    )
)
#
# M <- matrix(c(
#     0, 0,
#     1, -1,
#     0, 1
# ), nrow = 3, byrow = T)
#
# L <- matrix(c(
#     1, -1, 1,
#     1, 0, -1
# ), ncol = 3, byrow = T)
#
# P <- matrix(c(
#     1, 0
# ), nrow = 1)
#
# M <- matrix(c(
#     0,# 0,
#     1,# -1,
#     0#, 1
# ), nrow = 3, byrow = T)
#
# L <- matrix(c(
#     # 1, -1, 1,
#     1, 0, -1
# ), ncol = 3, byrow = T)
#
# P <- matrix(c(
#     0, 1
# ), nrow = 1)
