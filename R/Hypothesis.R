#' Hypothesis class
#' @description Some description
#' @export
#' @importFrom R6 R6Class
Hypothesis <- R6::R6Class(
    "Hypothesis",
    private = list(
        G = matrix(),
        M = matrix(),
        L = matrix(),
        K = matrix(),
        description = "",
        contrast_names = "",

        #' @description Validates the input for different functions.
        #' @importFrom stringr str_glue
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

            if (is.null(private$G)) {
                if (length(lgc$expose$group_labels) == 1L) {
                    private$G <- matrix(1, ncol = 1, nrow = 1)
                } else {
                    stop("G matrix must be provided")
                }
            } else if (ncol(private$G) != length(lgc$expose$group_labels)) {
                stop("Number of columns of the G matrix must be equal to the number of groups")
            }

            if (is.null(private$L)) {
                if (length(lgc$expose$covariates) == 0L) {
                    private$L <- matrix(1, ncol = 1, nrow = 1)
                } else {
                    stop("L matrix must be provided")
                }
            } else if (ncol(private$L) != (length(lgc$expose$covariates) + 1)) {
                stop("Number of columns of the L matrix must be equal to the number of covariates + 1.")
            }


            dim_K <- helper$calculate_K_matrix_dimensions(
                G = private$G,
                L = private$L,
                M = private$M
            )

            if (is.null(private$K)) {
                private$K <- array(0, dim = dim_K)
            } else if (any(dim(private$K) != dim_K)) {
                stop(str_glue("The dimension of the K matrix do must be equal to {paste0(dim_K, collapse = ' x ')}."))
            }
        }
    ),
    active = list(

        #' @field get_G Return the G matrix of the hypothesis.
        get_G = function() {
            private$G
        },

        #' @field get_M Return the M matrix of the hypothesis.
        get_M = function() {
            private$M
        },

        #' @field get_L Return the L matrix of the hypothesis.
        get_L = function() {
            private$L
        },

        get_K = function() {
            private$K
        },

        #' @field get_description Return the description of the hypothesis.
        get_description = function() {
            private$description
        },

        #' @field get_description Return the description of the hypothesis.
        get_contrast_names = function() {
            private$contrast_names
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
        #' @param G G matrix. Can be omitted if the number of groups equals 1.
        #' @param L L matrix. Can be omitted if the number of covariates
        #' equals 0.
        #' @param description Character. Description of the hypothesis. Can be
        #' any character.
        initialize = function(M = NULL, G = NULL, L = NULL, K = NULL, description = "", contrast_names = "") {
            private$M <- M
            private$G <- G
            private$L <- L
            private$K <- K
            private$description <- description
            private$contrast_names <- contrast_names
        },

        #' @importFrom dplyr case_when
        get_wald_string = function(lgc) {
            private$validate(lgc)

            B_array <- lgc$B_array_est
            B_array_labels <- lgc$B_array_labels
            B_vec_labels <- as.vector(B_array_labels)
            contrast_matrix <- helper$construct_R_matrix(
                B_array,
                L = self$get_L,
                G = self$get_G,
                M = self$get_M
            )$R
            K <- as.vector(self$get_K)
            constraints <- apply(contrast_matrix, 1, function(myrow) {
                labels <- B_vec_labels[myrow != 0]
                myrow <- myrow[myrow != 0]
                constraint <- case_when(
                    myrow == 1 ~ paste0("+", labels),
                    myrow == -1 ~ paste0("-", labels),
                    myrow > 0 ~  paste0("+", myrow, "*", labels),
                    TRUE ~  paste0(myrow, "*", labels)
                )
                if (myrow[1] == 1) {
                    constraint[1] <- labels[1]
                } else if (myrow[1] > 0) {
                    constraint[1] <- paste0(myrow[1], "*", labels[1])
                }

                paste0(constraint, collapse = "")
            })
            constraints <- sapply(1:length(constraints), function(index) {
                paste0(constraints[index], " == ", K[index])
            })
            paste0(constraints, collapse = "\n")
        },


        #' @description Returns the a string for the Wald test.
        #' @export
        #' @param lgc Object of class Lgc.
        get_wald_string_deprecated = function(lgc) {

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
                    paste0("(", x, ")*(", y, ")") |>
                        paste0(collapse = " + ") |>
                        sapply(Deriv::Simplify)
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

            result2 <- array(NA, dim = c(nrow(private$G), dim(result1)[2], dim(par_labels)[3]))

            for (i in 1:dim(result2)[3]) {
                result2[,,i] <- mx(private$G, result1[,,i])
            }

            result3 <- array(NA, dim = c(dim(result2)[1], dim(result2)[2], nrow(private$L)))

            for (i in 1:dim(result3)[1]) {
                for (j in 1:dim(result3)[2]) {
                    for (k in 1:dim(result3)[3]) {
                        result3[i,j,k] <- mx(private$L[k,,drop=F], matrix(result2[i,j,],ncol=1))
                    }
                }
            }

            apply(result3, 3, function(mat) {
                apply(mat, 2, function(vec) {
                    vec |>
                        sapply(function(x) paste0(x, " == 0")) |>
                        paste0(collapse = "\n")
                }) |> paste0(collapse = "\n")
            }) |> paste0(collapse = "\n")
        },

        #' @export
        get_contrasts = function(lgc) {
            par_labels <- lgc$get_par_labels
            M <- self$get_M
            G <- self$get_G
            L <- self$get_L

            res <- lapply(1:ncol(M), function(col_index_M) {
                # col_index_M <- 1
                col_M <- M[,col_index_M]
                sapply(1:nrow(G), function(row_index_G) {
                    # row_index_G <- 1
                    row_G <- G[row_index_G,]
                    between_within <- t(sapply(row_G, function(cell_G) cell_G * col_M))
                    lapply(1:nrow(L), function(row_index_L) {
                        # row_index_L <- 1
                        row_L <- L[row_index_L,]
                        sapply(row_L, function(cell_L) cell_L * between_within, simplify = "array")
                    })
                })
            })

            res <- res |>
                unlist(recursive = F) |>
                lapply(c) |>
                (\(x) do.call(cbind, x))()
            rownames(res) <- par_labels
            res
        },

        #' @importFrom Deriv Simplify
        #' @export
        get_contrasts_old = function(lgc) {
            simplify_cont <- function(cont) {
                # cont <- "1*.beta_6_6_1+1*.beta_6_6_6-1.2*(1*.beta_6_6_2+1*.beta_6_6_5)"
                while(str_detect(cont, "\\([^\\)]+\\.(beta|alpha)[^\\)]+\\)")) {
                    to_be_replaced <- str_extract(cont, "(\\+|\\-)*[0-9]+(\\.[0-9]+)*\\*\\([^\\)]+\\)")
                    multiplyer <- str_extract(to_be_replaced, "^(\\+|\\-)*[0-9]+(\\.[0-9]+)*") |>
                        str_replace("^((\\+|\\-)*[0-9]+(\\.[0-9]+)*)", "(\\1)")
                    factors <- to_be_replaced |>
                        str_replace("^(\\+|\\-)*[0-9]+(\\.[0-9]+)*\\*\\(", "") |>
                        str_replace("\\)$", "") |>
                        str_extract_all("(\\+|\\-)*[0-9]+(\\.[0-9]+)*\\*\\.(beta|alpha)[a-z0-9_]+") |>
                        unlist() |>
                        str_replace("^((\\+|\\-)*[0-9]+(\\.[0-9]+)*)", "(\\1)") |>
                        (\(x) {
                            # multiplyer <- "(-1.2)"
                            # x <- "(+1)*.beta_6_6_5"
                            parameter <- str_extract(x, "\\.(beta|alpha)[0-9_]+")
                            multiplyer_inner <- str_extract(x, "^\\(((\\+|\\-)*[0-9]+(\\.[0-9]+)*)\\)")
                            new_multiplyer <- as.numeric(Deriv::Simplify(paste0(multiplyer, "*", multiplyer_inner)))
                            paste0(
                                if (new_multiplyer>0) "+" else "",
                                new_multiplyer,
                                "*",
                                parameter
                            )
                        })() |>
                        paste0(collapse = "")
                    cont <- str_replace(cont, fixed(to_be_replaced), fixed(factors))
                }
                cont
            }

            par_labels <- c(lgc$get_par_labels)
            wald_string <- self$get_wald_string(lgc)
            conts <- wald_string |>
                str_replace_all(" ", "") |>
                str_split("\n") |>
                unlist() |>
                str_replace_all("==0", "") |>
                str_replace_all("\\+(\\.beta|\\.alpha)", "+1*\\1") |>
                str_replace_all("\\-(\\.beta|\\.alpha)", "-1*\\1") |>
                str_replace_all("\\+\\(", "+1*(") |>
                str_replace_all("\\-\\(", "-1*(") |>
                str_replace_all("\\((\\.beta|\\.alpha)", "(+1*\\1") |>
                str_replace_all("\\((\\.beta|\\.alpha)", "(-1*\\1") |>
                str_replace_all("^(\\.beta|\\.alpha)", "1*\\1")

            conts_names <- sapply(conts, Deriv::Simplify) |> as.character()

            conts <- sapply(conts, simplify_cont)
            conts <- lapply(conts, function(cont) {
                # cont <- "1*.beta_5_2_2-1*.beta_5_2_1"
                factors <- str_extract_all(cont, "(\\+|\\-)*[0-9]+(\\.[0-9]+)*\\*\\.(beta|alpha)([a-z0-9\\_]+)") |> unlist()
                multiplyers <- str_extract_all(factors, "(\\+|\\-)*[0-9]+(\\.[0-9]+)*\\*") |>
                    str_replace("\\*", "") |>
                    unlist() |> as.numeric()
                parameters <- str_extract_all(factors, "\\.(beta|alpha)[a-z0-9_]+") |> unlist()
                multiplyers <- sapply(par_labels, function(x) if (!(x %in% parameters)) 0 else multiplyers[which(x == parameters)])
            }) |>
                (\(x) do.call(cbind, x))()
            colnames(conts) <- conts_names
            conts
        }
    )
)
