#' @keywords internal
parse_sphericity_formula <- function(sphericity, idesign) {
    model_terms <- get_term_names(idesign)
    if (is.logical(sphericity) && sphericity) {
        return(1:length(model_terms))
    } else if (is.logical(sphericity) && !sphericity) {
        return(integer(0))
    } else {

        lhs <- as.logical(extract_dep_var(sphericity))
        rhs <- get_term_names(sphericity)
        if (!all(rhs %in% model_terms)) {
            stop("Some terms in the sphericity formula do not appear in the idesign formula.")
        }
        rhs_indices <-
            sapply(rhs, function(term)
                which(term == model_terms))
        if (lhs) {
            rhs_indices <- (1:length(model_terms))[-rhs_indices]
        }
        return(rhs_indices)
    }
}

#' @keywords internal
get_comp_sym_constraints <- function(mod) {
    lvs      <- mod@lvs
    dvs      <- mod@dvs

    nlvs <- length(lvs)
    ndvs <- length(dvs)


    # 1. constrain (co)variances of lvs to zero
    for (lhs in 1:nlvs) {
        for (rhs in 1:lhs) {
            mod@variances <- add_parameter(
                mod@variances,
                lhs   = lvs[lhs],
                op    = "~~",
                rhs   = lvs[rhs],
                free  = 0,
                value = 0
            )
        }
    }

    # 1. constrain (co)variances of dvs
    for (lhs in 1:ndvs) {
        for (rhs in 1:lhs) {
            if (lhs == rhs) {
                # variances
                mod@variances <- add_parameter(
                    mod@variances,
                    lhs   = dvs[lhs],
                    op    = "~~",
                    rhs   = dvs[rhs],
                    free  = 0,
                    value = "sigma_var"
                )
            } else {
                # covariances
                mod@variances <- add_parameter(
                    mod@variances,
                    lhs   = dvs[lhs],
                    op    = "~~",
                    rhs   = dvs[rhs],
                    free  = 0,
                    value = "sigma_covar"
                )
            }
        }
    }

    return(mod)
}

#' @keywords internal
get_sphericity_constraints <- function(mod) {
    lvs      <- mod@lvs
    dvs      <- mod@dvs

    nlvs <- length(lvs)
    ndvs <- length(dvs)

    hypotheses <- mod@hypotheses
    sphericity <- get_option(mod, "sphericity")

    # 1. constrain (co)variances of dvs to zero
    for (lhs in 1:ndvs) {
        for (rhs in 1:lhs) {
            mod@variances <- add_parameter(
                mod@variances,
                lhs   = dvs[lhs],
                op    = "~~",
                rhs   = dvs[rhs],
                free  = 0,
                value = 0
            )
        }
    }

    # 1. constrain (co)variances of lvs
    for (lhs in 1:nlvs) {
        for (rhs in 1:lhs) {
            ## do lhs and rhs appear in the same hypothesis?
            in_hypothesis <-
                which(sapply(hypotheses, function(hypothesis)
                    lhs %in% hypothesis$indices & rhs %in% hypothesis$indices))

            if (length(in_hypothesis) > 1L) {
                ## lhs and rhs appear in more than one hypothesis
                ## -> error
                stop(
                    paste0(
                        "Trying to impose sphericity. Some contrasts",
                        "appear in more than one hypothesis. I don't",
                        "know how to deal with this.",
                        collapse = " "
                    )
                )
            } else if (length(in_hypothesis) == 0) {
                ## lhs and rhs do NOT appear in the same hypothesis
                ## -> freely estimate
                mod@variances <- add_parameter(
                    mod@variances,
                    lhs   = lvs[lhs],
                    op    = "~~",
                    rhs   = lvs[rhs],
                    free  = 1,
                    value = NA
                )
            } else if (in_hypothesis %in% sphericity) {
                ## lhs and rhs DO appear in the same hypothesis
                ## -> constrain according to sphericity
                if (lhs == rhs) {
                    # variances
                    mod@variances <- add_parameter(
                        mod@variances,
                        lhs   = lvs[lhs],
                        op    = "~~",
                        rhs   = lvs[rhs],
                        free  = 0,
                        value = paste0("sigma_", in_hypothesis)
                    )
                } else {
                    # covariances
                    mod@variances <- add_parameter(
                        mod@variances,
                        lhs   = lvs[lhs],
                        op    = "~~",
                        rhs   = lvs[rhs],
                        free  = 0,
                        value = 0
                    )
                }
            } else {
                ## lhs and rhs DO appear in the same hypothesis
                ## but are not part of the sphericity constraint
                ## -> freely estimate
                mod@variances <- add_parameter(
                    mod@variances,
                    lhs   = lvs[lhs],
                    op    = "~~",
                    rhs   = lvs[rhs],
                    free  = 1,
                    value = NA
                )
            }
        }
    }

    return(mod)
}

#' @keywords internal
get_sphericity_constraints2 <- function(mod) {
    lvs      <- mod@lvs
    dvs      <- mod@dvs

    nlvs <- length(lvs)
    ndvs <- length(dvs)

    hypotheses <- mod@hypotheses
    sphericity <- get_option(mod, "sphericity")

    # 1. constrain (co)variances of dvs to zero
    for (lhs in 1:ndvs) {
        for (rhs in 1:lhs) {
            mod@variances <- add_parameter(
                mod@variances,
                lhs   = dvs[lhs],
                op    = "~~",
                rhs   = dvs[rhs],
                free  = 0,
                value = 0
            )
        }
    }

    # 1. constrain (co)variances of lvs
    for (lhs in 1:nlvs) {
        for (rhs in 1:lhs) {
            if (lhs != rhs) {
                mod@variances <- add_parameter(
                    mod@variances,
                    lhs   = lvs[lhs],
                    op    = "~~",
                    rhs   = lvs[rhs],
                    free  = 1,
                    value = NA
                )
            } else {
                ## does contrast appear in more than one hypothesis?
                in_hypothesis <-
                    which(sapply(hypotheses, function(hypothesis)
                        lhs %in% hypothesis$indices))

                if (length(in_hypothesis) > 1) {
                    ## lhs appears in more than one hypothesis
                    ## -> error
                    stop("Trying to impose sphericity. Some contrasts ",
                         "appear in more than one hypothesis. I don't ",
                         "know how to deal with this.")
                } else if (in_hypothesis %in% sphericity) {
                    mod@variances <- add_parameter(
                        mod@variances,
                        lhs   = lvs[lhs],
                        op    = "~~",
                        rhs   = lvs[lhs],
                        free  = 0,
                        value = paste0("sigma_", in_hypothesis)
                    )
                } else {
                    mod@variances <- add_parameter(
                        mod@variances,
                        lhs   = lvs[lhs],
                        op    = "~~",
                        rhs   = lvs[rhs],
                        free  = 1,
                        value = NA
                    )
                }
            }
        }
    }

    return(mod)
}

#' @keywords internal
get_unconstrainted_cov <- function(mod) {
    dvs <- mod@dvs
    ndvs <- length(dvs)

    # 1. constrain (co)variances of dvs to zero
    for (lhs in 1:ndvs) {
        for (rhs in 1:lhs) {
            mod@variances <- add_parameter(mod@variances,
                                           lhs   = dvs[lhs],
                                           op    = "~~",
                                           rhs   = dvs[rhs],
                                           free  = 0,
                                           value = 0)
        }
    }

    return(mod)
}
