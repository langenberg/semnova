#' Specifying a measurement model.
#' @export
#' @param ... Named arguments each representing a latent variable. The arguments
#' are character vectors containing the variable names the latent variables are measured by.
#' @param list List. Each list element represents a latent variable.
#' List elements are character vectors containing the variable names the latent variables are measured by.
#' @param lv_scaling Character vector. Defines the strategy for latent variable scaling.
#' Default is \code{lv_scaling = "effect"}. Possible strategies are: \code{c("effect", "referent")}.
#' @param invariance Not yet implemented.
#' @return Object of classe \code{mmodel}.
#' @examples
#'
#' mmodel <- create_mmodel(
#'     A1B1 = "var1",
#'     A2B1 = "var2",
#'     A3B1 = "var3",
#'     A1B2 = "var4",
#'     A2B2 = "var5",
#'     A3B2 = "var6",
#'     lv_scaling = "referent"
#' )
#'
create_mmodel <- function(..., list = NULL, lv_scaling = "effect", invariance = NULL) {
    dots <- list(...)
    mmodel <- c(dots, list)
    if (length(mmodel) == 0L) {
        stop("No variable names have been provided.")
    }
    if (is.character(mmodel) & length(mmodel) == 1L) {
        result <- list(list = mmodel,
                       lv_scaling = lv_scaling,
                       invariance = invariance)
        attr(result, "append") <- TRUE
        class(result) <- "mmodel"
        return(result)
    } else {
        # if (is.null(names(mmodel)) | (any(names(mmodel) == ""))) {
        #     stop("At least one provided variable does not have a name.")
        # }
        if (!(lv_scaling %in% c("referent","effect"))) {
            stop(paste0("Unknown strategy to scale indicator loading [lv_scaling = ", lv_scaling, "]"))
        }
        result <- list(list = mmodel,
                       lv_scaling = lv_scaling,
                       invariance = invariance)
        attr(result, "append") <- FALSE
        class(result) <- "mmodel"
        return(result)
    }
}

#' @keywords internal
specify_measurement_model <- function(mod, mmodel) {
    if (attr(mmodel, "append")) {
        mod@append <- paste0(mod@append, "\n",
                             mmodel$list)
        mod@dvs <- mmodel$dv_names
        return(mod)
    }

    lv_scaling <- mmodel$lv_scaling
    mod@indicators <- unlist(mmodel$list)
    variables <- names(mmodel$list)

    ## loadings

    if (lv_scaling == "referent") {
        for (var_index in 1:length(variables)) {
            for (indicator in 1:length(mmodel$list[[var_index]])) {
                if (indicator == 1) {
                    mod@mmodel <- add_parameter(mod@mmodel,
                                                lhs   = variables[var_index],
                                                op    = "=~",
                                                rhs   = mmodel$list[[var_index]][indicator],
                                                free  = 0,
                                                value = 1)

                    mod@intercepts <- add_parameter(mod@intercepts,
                                                    lhs   = mmodel$list[[var_index]][indicator],
                                                    op    = "~",
                                                    rhs   = 1,
                                                    free  = 0,
                                                    value = 0)
                } else {
                    mod@mmodel <- add_parameter(mod@mmodel,
                                                lhs   = variables[var_index],
                                                op    = "=~",
                                                rhs   = mmodel$list[[var_index]][indicator],
                                                free  = 0,
                                                value = paste0(".l", indicator))

                    mod@intercepts <- add_parameter(mod@intercepts,
                                                    lhs   = mmodel$list[[var_index]][indicator],
                                                    op    = "~",
                                                    rhs   = 1,
                                                    free  = 0,
                                                    value = paste0(".i", indicator))
                }
            }
        }
    } else if (lv_scaling == "effect") {
        for (var_index in 1:length(variables)) {
            for (indicator in 1:length(mmodel$list[[var_index]])) {
                mod@mmodel <- add_parameter(mod@mmodel,
                                            lhs   = variables[var_index],
                                            op    = "=~",
                                            rhs   = mmodel$list[[var_index]][indicator],
                                            free  = 0,
                                            value = paste0(".l", indicator),
                                            na    = if (indicator==1) 1 else 0)

                mod@intercepts <- add_parameter(mod@intercepts,
                                                lhs   = mmodel$list[[var_index]][indicator],
                                                op    = "~",
                                                rhs   = 1,
                                                free  = 0,
                                                value = paste0(".i", indicator))
            }
        }
        nindicators <- length(mmodel$list[[1]])
        indicators <- paste0(".l", 1:nindicators)
        constraint <- paste0(indicators, collapse = " + ")
        mod@constraints <- add_parameter(mod@constraints,
                                         lhs   = constraint,
                                         op    = "==",
                                         rhs   = nindicators,
                                         free  = 1,
                                         value = NA,
                                         group = 0)
        indicators <- paste0(".i", 1:nindicators)
        constraint <- paste0(indicators, collapse = " + ")
        mod@constraints <- add_parameter(mod@constraints,
                                         lhs   = constraint,
                                         op    = "==",
                                         rhs   = 0,
                                         free  = 1,
                                         value = NA,
                                         group = 0)
    }

    return(mod)
}


#' @keywords internal
specify_intercepts_indicator <- function(mod, mmodel) {
    # if (attr(mmodel, "append")) {
    #     return(mod)
    # }
    #
    # ndvs <- length(mod@dvs)
    # nindicators <- length(mmodel$list[[1]])
    #
    # ## intercepts indicators
    # for (dv in 1:ndvs) {
    #     for (indicator in 1:nindicators) {
    #         if (indicator == 1) {
    #             mod@intercepts <- add_parameter(mod@intercepts,
    #                                             lhs   = mmodel$list[[dv]][indicator],
    #                                             op    = "~",
    #                                             rhs   = 1,
    #                                             free  = 0,
    #                                             value = 0)
    #         } else {
    #             mod@intercepts <- add_parameter(mod@intercepts,
    #                                             lhs   = mmodel$list[[dv]][indicator],
    #                                             op    = "~",
    #                                             rhs   = 1,
    #                                             free  = 0,
    #                                             value = paste0(".i", indicator))
    #         }
    #     }
    # }
    return(mod)
}

#' @keywords internal
specify_intercepts_dv <- function(mod) {

    ndvs <- length(mod@dvs)
    dvs  <- mod@dvs

    ## intercepts dvs

    for (dv in 1:ndvs) {
        mod@intercepts <- add_parameter(mod@intercepts,
                                        lhs   = dvs[dv],
                                        op    = "~",
                                        rhs   = 1,
                                        free  = 0,
                                        value = 0)
    }


    return(mod)
}



#' @keywords internal
specify_structural_coefficients <- function(mod) {
    dvs <- mod@dvs
    C_matrix <- mod@C_matrix

    if (nrow(C_matrix) == ncol(C_matrix)) {
        B <- solve(C_matrix)
    } else {
        B <- MASS::ginv(C_matrix)
    }

    for (lv in 1:ncol(B)) {
        for (dv in 1:length(dvs)){
            mod@struc_coeff <- add_parameter(mod@struc_coeff,
                                             lhs   = paste0(".pi", lv),
                                             op    = "=~",
                                             rhs   = dvs[dv],
                                             free  = 0,
                                             value = B[dv,lv])
        }
    }
    return(mod)
}


#' @keywords internal
specify_variances <- function(mod) {

    # dvs <- mod@dvs
    #
    # ## intercepts dvs
    #
    # for (dv in 1:length(dvs)) {
    #     mod@variances <- add_parameter(mod@variances,
    #                                    lhs   = dvs[dv],
    #                                    op    = "~~",
    #                                    rhs   = dvs[dv],
    #                                    free  = 0,
    #                                    value = 0)
    # }

    if (get_option(mod, "compound_symmetry")) {
        return(get_comp_sym_constraints(mod))
    } else if (length(get_option(mod, "sphericity")) > 0L) {
        return(get_sphericity_constraints(mod))
    } else {
        return(get_unconstrainted_cov(mod))
    }
}

#' @keywords internal
specify_regressions <- function(mod) {

    nlvs <- nrow(mod@C_matrix)

    ## intercepts dvs

    for (lv in 1:nlvs) {
        mod@regressions <- add_parameter(mod@regressions,
                                         lhs   = paste0(".pi", lv),
                                         op    = "~",
                                         rhs   = 1,
                                         free  = 0,
                                         value = paste0(".m", lv))
    }

    mod@lv_labels <- paste0(".m", 1:nlvs)
    mod@lvs       <- paste0(".pi", 1:nlvs)

    return(mod)
}


