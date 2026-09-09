#' Kronecker contrast matrix construction
#'
#' @description
#' Builds the contrast matrix `R` that vectorizes a general linear
#' hypothesis `H0: G . (L . B_g . M) = K` for a multigroup, multivariate
#' model, per the derivation in `vignettes/glh.Rmd`:
#'
#' \deqn{R = (G \otimes I_{hr}) (I_{n_{groups}} \otimes (M^\top \otimes L))}
#'
#' applied to `vec(B)`, the stacked per-group coefficient arrays.
#'
#' @param B_array Numeric array, dimensions `(p+1, q, n_groups)`, the
#'   stacked per-group coefficient matrices (predictors-incl-intercept x
#'   repeated-measures x groups).
#' @param G Numeric matrix, `d x n_groups`, the group contrast matrix.
#' @param L Numeric matrix, `h x (p+1)`, the predictor contrast matrix.
#' @param M Numeric matrix, `q x r`, the within-subject contrast matrix.
#'
#' @return A list with `R` (the `(d*h*r) x (n_groups*(p+1)*q)` contrast
#'   matrix) and `dim` (its dimensions, named `c("rows", "cols")`).
#' @export
contrast_matrix <- function(G, L, M, B_array = NULL) {
    if (!is.matrix(G) || !is.matrix(L) || !is.matrix(M)) {
        stop("semnova: G, L, and M must all be matrices", call. = FALSE)
    }

    h <- nrow(L)
    r <- ncol(M)
    d <- nrow(G)
    n_groups <- ncol(G)

    Rg <- kronecker(t(M), L)
    R_within <- kronecker(diag(n_groups), Rg)
    R <- kronecker(G, diag(h * r)) %*% R_within

    dim_out <- c(rows = d * h * r, cols = n_groups * ncol(L) * nrow(M))
    names(dim_out) <- c("rows", "cols")

    if (!is.null(B_array)) {
        if (dim(B_array)[1] != ncol(L) || dim(B_array)[2] != nrow(M) || dim(B_array)[3] != n_groups) {
            stop(
                "semnova: B_array dimensions (", paste(dim(B_array), collapse = " x "),
                ") are not compatible with L/M/G (expected ", ncol(L), " x ", nrow(M), " x ", n_groups, ")",
                call. = FALSE
            )
        }
        return(list(R = R, dim = dim_out, vec_B = as.vector(B_array)))
    }

    list(R = R, dim = dim_out)
}
