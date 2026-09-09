#' Reshape a fit's pi-layer coefficients into contrast-ready arrays
#'
#' @description
#' Pipeline stage 2: a pure reshape of [model_parameters()]'s output (it
#' never re-reads the `lavaan` object itself) into the arrays the contrast
#' math needs. Which parameter belongs in which cell is resolved through
#' [regression_labels()]. That is the same function that generated the labels when the
#' syntax was written, so construction and read-back cannot drift apart.
#'
#' @param fit An [Lgc] object.
#'
#' @return A list with:
#'   \describe{
#'     \item{`B_array`}{`(covariate + 1) x pi x group` array of point
#'       estimates; the first covariate slice is the pi intercepts.}
#'     \item{`B_vec`}{`as.vector(B_array)`, named by parameter label.}
#'     \item{`B_vec_vcov`}{Sampling covariance matrix of `B_vec`.}
#'     \item{`B_var_vec`}{Between-person (co)variance of the pi's, laid out
#'       over the same vec positions, the interindividual-differences
#'       matrix used for SD bands. Zero wherever a position is not a pi
#'       intercept, or two positions are in different groups.}
#'     \item{`labels`}{The label array `B_array` was filled from.}
#'   }
#' @export
regression_coefficients <- function(fit) {
    if (!inherits(fit, "Lgc")) {
        stop_semnova("`fit` must be an Lgc object (see lgc() or semnova())")
    }

    labels <- regression_labels(
        pis = fit$pis,
        covariates = names(fit$covariates_mmodel),
        groups = fit$group_labels
    )

    mp <- model_parameters(fit)
    est_by_label <- stats::setNames(mp$est, mp$label)

    missing <- setdiff(as.vector(labels), names(est_by_label))
    if (length(missing) > 0L) {
        stop_semnova(
            "fitted model is missing expected pi-layer parameter(s): ",
            paste(utils::head(missing, 5L), collapse = ", "),
            if (length(missing) > 5L) paste0(" (and ", length(missing) - 5L, " more)") else ""
        )
    }

    B_array <- array(est_by_label[as.vector(labels)], dim = dim(labels), dimnames = dimnames(labels))
    B_vec <- as.vector(B_array)
    label_vec <- as.vector(labels)
    names(B_vec) <- label_vec

    vcov_full <- lavaan::vcov(fit$sem_obj)
    B_vec_vcov <- vcov_full[label_vec, label_vec, drop = FALSE]
    dimnames(B_vec_vcov) <- list(label_vec, label_vec)

    # between-person (co)variance of the pi's over the same vec positions:
    # non-zero only where both positions are pi intercepts in the same group
    parsed <- lapply(label_vec, parse_param_label)
    n <- length(label_vec)
    B_var_vec <- matrix(0, nrow = n, ncol = n, dimnames = list(label_vec, label_vec))

    for (i in seq_len(n)) {
        pi_i <- parsed[[i]]
        if (!identical(pi_i$type, "pi_intercept")) next
        for (j in i:n) {
            pi_j <- parsed[[j]]
            if (!identical(pi_j$type, "pi_intercept")) next
            if (pi_i$group != pi_j$group) next

            lo <- min(pi_i$pi, pi_j$pi)
            hi <- max(pi_i$pi, pi_j$pi)
            var_label <- if (lo == hi) {
                param_label("pi_var", pi = lo, group = pi_i$group)
            } else {
                param_label("pi_cov", pi = lo, pi2 = hi, group = pi_i$group)
            }
            value <- est_by_label[var_label]
            if (!is.na(value)) {
                B_var_vec[i, j] <- B_var_vec[j, i] <- value
            }
        }
    }

    list(
        B_array = B_array,
        B_vec = B_vec,
        B_vec_vcov = B_vec_vcov,
        B_var_vec = B_var_vec,
        labels = labels
    )
}
