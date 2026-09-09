#' @keywords internal
#' @noRd
is_latent_variable <- function(fit, variables) {
    latent <- c(
        names(fit$etas_mmodel)[vapply(fit$etas_mmodel, length, integer(1L)) > 1L],
        fit$pis,
        names(fit$covariates_mmodel)[vapply(fit$covariates_mmodel, length, integer(1L)) > 1L]
    )
    variables %in% latent
}

#' Read a fitted model's parameters
#'
#' @description
#' Pipeline stage 1: the one canonical read of the fitted `lavaan` object.
#' Everything that needs to know what's in a fitted model, printing,
#' plotting, and [regression_coefficients()] (stage 2), goes through
#' this, so there is exactly one place that turns a `lavaan` fit into
#' semnova's own view of it.
#'
#' `type` is classified structurally, from the operator and whether the
#' variables involved are latent, rather than by parsing labels, a fixed
#' parameter carries no label at all, so structure is the only thing every
#' row has.
#'
#' Every label in the model is now the engine's, including the measurement
#' layer's (see [relabel_measurement_syntax()]), so each row also carries
#' its parsed identity: `semnova_type` and the index columns. An index of
#' `0` means the parameter is one shared parameter across that whole
#' dimension, the equality constraint an invariance level imposes.
#'
#' @param fit An [Lgc] object.
#' @param what Character vector of `type` values to keep, or `"all"`.
#'
#' @return A data.frame with `lhs`, `op`, `rhs`, `group`, `group_index`,
#'   `label`, `est`, `se`, `pvalue`, `free`, `type`, `semnova_type`, and the
#'   parsed index columns `construct`, `construct2`, `indicator`,
#'   `indicator2`, `pi`, `pi2`, `covariate` (`NA` where the row has no
#'   label, `0` where the parameter is shared across that dimension).
#' @export
model_parameters <- function(fit, what = "all") {
    if (!inherits(fit, "Lgc")) {
        stop_semnova("`fit` must be an Lgc object (see lgc() or semnova())")
    }

    pt <- lavaan::parTable(fit$sem_obj)
    pt <- as.data.frame(pt, stringsAsFactors = FALSE)

    if (is.null(pt$label)) {
        pt$label <- ""
    }
    pt$label[is.na(pt$label)] <- ""
    if (is.null(pt$se)) {
        pt$se <- NA_real_
    }

    lhs_latent <- is_latent_variable(fit, pt$lhs)
    same <- pt$lhs == pt$rhs

    type <- rep(NA_character_, nrow(pt))
    type[pt$op == "=~"] <- "measurement"
    type[pt$op == "~1"] <- "intercept"
    type[pt$op == "~"] <- "regression"
    type[pt$op == "=="] <- "constraint"
    type[pt$op == ":="] <- "definition"
    type[pt$op == "~~" & same & lhs_latent] <- "variance"
    type[pt$op == "~~" & same & !lhs_latent] <- "resid_var"
    type[pt$op == "~~" & !same & lhs_latent] <- "covariance"
    type[pt$op == "~~" & !same & !lhs_latent] <- "resid_cov"

    group_index <- if (is.null(pt$group)) rep(1L, nrow(pt)) else as.integer(pt$group)
    group <- ifelse(
        group_index >= 1L & group_index <= length(fit$group_labels),
        fit$group_labels[pmax(group_index, 1L)],
        NA_character_
    )

    parsed <- lapply(pt$label, parse_param_label)
    pick <- function(field) {
        vapply(parsed, function(p) {
            v <- p[[field]]
            if (is.null(v)) NA_integer_ else as.integer(v)
        }, integer(1L))
    }

    out <- data.frame(
        lhs = pt$lhs,
        op = pt$op,
        rhs = pt$rhs,
        group = group,
        group_index = group_index,
        label = pt$label,
        est = pt$est,
        se = pt$se,
        pvalue = 2 * stats::pnorm(-abs(pt$est / pt$se)),
        free = pt$free != 0L,
        type = type,
        semnova_type = vapply(parsed, function(p) p$type, character(1L)),
        construct = pick("construct"),
        construct2 = pick("construct2"),
        indicator = pick("indicator"),
        indicator2 = pick("indicator2"),
        pi = pick("pi"),
        pi2 = pick("pi2"),
        covariate = pick("covariate"),
        stringsAsFactors = FALSE
    )

    if (!identical(what, "all")) {
        check_character(what, "what")
        out <- out[!is.na(out$type) & out$type %in% what, , drop = FALSE]
    }

    out
}
