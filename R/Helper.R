
#' Helper class
#' @description Some description
#' @noRd
#' @importFrom R6 R6Class
Helper <- R6::R6Class(
    "Helper",
    private = list(
        #' @description Helper function.
        #' @keywords internal
        list_to_str = function(
            l,
            sep = ", ",
            before_element = "",
            after_element = "",
            before_name = "",
            ...
        ) {

            if (is.null(l)) {
                return("NULL")
            } else if (length(l) == 0L) {
                return("list()")
            }


            l_names <- names(l)
            names(l) <- NULL
            l <- sapply(l, self$to_str)
            l <- paste0(before_element, l, after_element)

            if (!is.null(l_names) && all(l_names == "")) {
                l_names <- NULL
            }

            if (!is.null(l_names)) {
                l <- sapply(1:length(l), function(index) {
                    if (l_names[index] == "") {
                        l[index]
                    } else {
                        paste0(before_name, l_names[index], " = ", l[index])
                    }
                })
            }


            l <- paste0("list(", paste0(l, collapse = ", "), ")")

            l
        },

        #' @description Helper function.
        #' @keywords internal
        vector_to_str = function(vec, default = character, ...) {
            empty <- function() {
                case_when(
                    inherits(default(), "character") ~ "character()",
                    inherits(default(), "numeric") ~ "numeric()",
                    inherits(default(), "logical") ~ "logical()"
                )
            }

            if (is.null(vec) || length(vec) == 0L) {
                "NULL"
            } else if (length(vec) == 0L) {
                empty()
            } else if (is.numeric(vec) || is.logical(vec)) {
                private$vector_to_str_numeric(vec)
            } else if (is.character(vec)) {
                private$vector_to_str_character(vec)
            } else {
                stop("Unknown vector class.")
            }
        },

        #' @description Helper function.
        #' @keywords internal
        vector_to_str_character = function(vec, ...) {

            vec <- sapply(vec, function(x) paste0('"', x, '"'), USE.NAMES = FALSE)

            private$vector_to_str_numeric(vec)
        },

        #' @description Helper function.
        #' @keywords internal
        vector_to_str_numeric = function(vec, ...) {

            vec_names <- names(vec)

            if (!is.null(vec_names) && all(vec_names == "")) {
                vec_names <- NULL
            }

            if (!is.null(vec_names)) {
                vec <- sapply(1:length(vec), function(index) {
                    if (vec_names[index] == "") {
                        vec[index]
                    } else {
                        paste0(vec_names[index], " = ", vec[index])
                    }
                })
            }

            if (length(vec) > 1L || !is.null(vec_names)) {
                vec <- paste0("c(", paste0(vec, collapse = ", "), ")")
            }

            vec
        },

        #' @description Helper function.
        #' @keywords internal
        matrix_to_str = function(mat, prefix = "", ...) {
            paste0(
                "matrix(c(\n",
                prefix, "    ",
                apply(mat, 1, paste0, collapse = ", ") |>
                    paste0(collapse = paste0(",\n", prefix, "    ")),
                "\n",
                prefix, "),", " nrow = ", nrow(mat), ", byrow = T)"
            )
        },

        #' @description Helper function.
        #' @keywords internal
        hypothesis_to_str = function(hypothesis, ...) {
            description <- hypothesis$get_description
            M <- hypothesis$get_M
            L <- hypothesis$get_L
            P <- hypothesis$get_P

            paste0(
                "Hypothesis$new(\n",
                if (description == "") "" else paste("description = ", self$to_str(description), ",\n"),
                paste0("M = ", self$to_str(M), ",\n"),
                paste0("L = ", self$to_str(L), ",\n"),
                paste0("P = ", self$to_str(P)),
                "\n)"
            )
        }
    ),

    active = list(

        #' @field expose Read only. Returns the private structure of the object. For debugging purposes only.
        expose = function() {
            private
        }
    ),

    public = list(
        #' @description Helper function.
        #' @keywords internal
        #' @importFrom styler style_text
        args_to_str = function(
            args,
            prefix = "",
            print_names = TRUE,
            sep = ",\n",
            ...
        ) {

            if (is.null(args) || length(args) == 0L) {
                return("")
            }

            results <- lapply(1:length(args), function(arg_index) {
                arg <- args[[arg_index]]
                arg_name <- names(args)[arg_index]
                if (is.null(arg_name) || length(arg_name) == 0L || arg_name == "" || !print_names) {
                    arg_name <- ""
                } else {
                    arg_name <- paste0(arg_name, " = ")
                }
                paste0(prefix, arg_name, self$to_str(arg))
            })

            results <- results[sapply(results, function(x) x != "")]

            results |> paste0(collapse = sep)
        },

        to_str = function(x, ...) {
            if (inherits(x, "Hypothesis")) {
                private$hypothesis_to_str(x, ...) |> styler::style_text() |> paste0(collapse = "\n")
            } else if (inherits(x, "list")) {
                private$list_to_str(x, ...) |> styler::style_text() |> paste0(collapse = "\n")
            } else if (inherits(x, "matrix")) {
                private$matrix_to_str(x, ...) |> styler::style_text() |> paste0(collapse = "\n")
            } else if (inherits(x, "numeric") || inherits(x, "character") || inherits(x, "logical")) {
                private$vector_to_str(x, ...) |> styler::style_text() |> paste0(collapse = "\n")
            } else {
                "unknown class"
            }
        },

        vec = function(obj) {
            vec_mat <- as.vector
            if (is.vector(obj)) {
                obj
            } else if (is.array(obj)) {
                if (length(dim(obj)) == 3) {
                    do.call(c, lapply(apply(obj, 3, identity, simplify = F), as.vector))
                } else if (length(dim(obj)) == 2) {
                    as.vector(obj)
                }
            } else {
                NULL
            }
        },

        calculate_K_matrix_dimensions = function(G, L, M) {
            d <- nrow(G)       # number of group contrasts
            h <- nrow(L)       # number of predictor contrasts
            r <- ncol(M)       # number of repeated measure contrasts

            return(c(d, h, r))  # dimensions of K
        },

        calculate_R_matrix_dimensions = function(G, L, M) {
            d <- nrow(G)       # group contrasts
            n_groups <- ncol(G)

            h <- nrow(L)       # predictor contrasts
            p1 <- ncol(L)

            r <- ncol(M)       # repeated measures contrasts
            q <- nrow(M)

            dim_R <- c(d * h * r, n_groups * p1 * q)
            names(dim_R) <- c("rows", "cols")
            return(dim_R)
        },

        construct_R_matrix = function(B_array, G, L, M) {
            p1 <- dim(B_array)[1]             # predictors + intercept
            q  <- dim(B_array)[2]             # repeated measures
            n_groups <- dim(B_array)[3]       # number of groups

            h <- nrow(L)                      # row contrasts
            r <- ncol(M)                      # column contrasts
            d <- nrow(G)                      # number of group contrasts

            # Vectorize B
            vec_B <- as.vector(B_array)

            # Build (M^T ⊗ L)
            Rg <- kronecker(t(M), L)  # (h*r) x (p1*q)

            # Block-diagonal for all groups
            R_within <- kronecker(diag(n_groups), Rg)  # (n_groups * h * r) x (n_groups * p1 * q)

            # Apply group contrast: (G ⊗ I_hr)
            R <- kronecker(G, diag(h * r)) %*% R_within  # (d * h * r) x (n_groups * p1 * q)

            return(list(
                R = R,
                vec_B = vec_B
            ))
        },

        construct_contrast_matrix_old_old = function(B_array, L, P, M) {
            C <- L
            L <- P

            # B_array: 3D array with dimensions [p+1, q, G]
            p1 <- dim(B_array)[1]  # p + 1 (predictors including intercept)
            q  <- dim(B_array)[2]  # repeated measures
            G  <- dim(B_array)[3]  # number of groups

            h <- nrow(L)           # number of row restrictions
            r <- ncol(M)           # number of column (within-subject) restrictions
            d <- nrow(C)           # number of between-group contrasts

            # Vectorize all B matrices (stacked group-wise)
            vec_B <- as.vector(B_array)  # length = G * p1 * q

            # (M^T ⊗ L): transforms vec(B_g) to vec(L B_g M), shape (h*r) x (p1*q)
            Rg <- kronecker(t(M), L)

            # Block diagonal for all groups: (I_G ⊗ (M^T ⊗ L))
            R_within <- kronecker(diag(G), Rg)  # shape: (G * h * r) x (G * p1 * q)

            # Group-level contrast: (I_{hr} ⊗ C)
            R <- kronecker(diag(h * r), C) %*% R_within  # shape: (d * h * r) x (G * p1 * q)

            return(list(
                R = R,
                vec_B = vec_B,
                dim_R = dim(R),
                dim_B = dim(B_array)
            ))
        },

        construct_contrast_matrix_old = function(B_array, L, P, M) {
            C <- L
            L <- P

            # Dimensions
            r <- dim(B_array)[1]  # rows in B_g (e.g., time points)
            c <- dim(B_array)[2]  # columns in B_g (e.g., predictors)
            G <- dim(B_array)[3]  # number of groups

            q <- nrow(L)          # rows of L (e.g., number of row contrasts)
            m0 <- ncol(M)         # columns of M (e.g., number of predictor contrasts)
            d <- nrow(C)          # number of group contrasts

            rc <- r * c           # number of elements per B_g
            Grc <- G * rc         # total number of elements in vec(B)

            # Base contrast for one group: (M^T ⊗ L)
            Rg <- kronecker(t(M), L)  # (q * m0) x (r * c)

            # Full block-diagonal for all groups
            R_within <- matrix(0, nrow = G * q * m0, ncol = Grc)
            for (g in 1:G) {
                row_idx <- ((g - 1) * q * m0 + 1):(g * q * m0)
                col_idx <- ((g - 1) * rc + 1):(g * rc)
                R_within[row_idx, col_idx] <- Rg
            }

            # Full contrast across groups: (I_{q*m0} ⊗ C)
            R <- kronecker(diag(q * m0), C) %*% R_within  # (d * q * m0) x (G * r * c)

            return(R)
        }
    )
)
