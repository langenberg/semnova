#' ParTable class
#' @description Some Description
#' @noRd
#' @importFrom R6 R6Class
ParTable <- R6::R6Class(
    "ParTable",
    private = list(
        par_table = NULL,

        #' @description Validates the input for different functions.
        #' @keywords internal
        #' @param lhs Character.
        #' @param op Character.
        #' @param rhs Character.
        #' @param free Logical.
        #' @param value Character.
        #' @param group Character.
        #' @param na Logical.
        #' @param remove_if_fixed Logical.
        validate_input = function(
            lhs,
            op,
            rhs,
            free,
            value,
            group,
            na,
            remove_if_fixed
        ) {

            input <- list()
            input$lhs <- as.character(lhs)
            input$op <- as.character(op)
            input$rhs <- as.character(rhs)
            input$free <- as.logical(free)
            input$value <- as.character(value)
            input$group <- as.character(group)
            input$na <- as.logical(na)
            input$remove_if_fixed <- as.logical(remove_if_fixed)

            input$lhs <- str_replace_all(input$lhs, " ", "")
            input$rhs <- str_replace_all(input$rhs, " ", "")
            input$op <- str_replace_all(input$op, " ", "")
            input$value <- str_replace_all(input$value, " ", "")

            input
        }
    ),
    active = list(

        #' @field get_par_table Returns the parameter table as tibble.
        get_par_table = function() {
            private$par_table
        }

    ),
    public = list(

        #' @description ParTable class constructor.
        #' @keywords internal
        initialize = function() {
            private$par_table <- tibble(
                id = numeric(),
                lhs = character(),
                op = character(),
                rhs = character(),
                free = logical(),
                value = character(),
                group = character(),
                na = logical(),
                remove_if_fixed = logical()
            )
        },

        #' @description Returns whether the ParTable instance is empty.
        #' @keywords internal
        is_empty = function() {
            nrow(private$par_table) == 0
        },

        #' @description Adds a parameter to the ParTable instance.
        #' @keywords internal
        #' @param lhs Character.
        #' @param op Character.
        #' @param rhs Character.
        #' @param free Logical.
        #' @param value Character.
        #' @param group Character.
        #' @param na Logical.
        #' @param remove_if_fixed Logical.
        add_parameter = function(
            lhs,
            op,
            rhs,
            value,
            free = TRUE,
            group = "1",
            na = FALSE,
            remove_if_fixed = F)
        {

            input <- private$validate_input(
                lhs = lhs,
                op = op,
                rhs = rhs,
                value = value,
                free = free,
                group = group,
                na = na,
                remove_if_fixed = remove_if_fixed
            )

            if (self$is_empty()) {
                new_id <- 1
            } else {
                new_id <- max(private$par_table$id) + 1
            }

            private$par_table <-
                private$par_table |>
                add_row(
                    tibble(
                        id = new_id,
                        lhs = input$lhs,
                        op = input$op,
                        rhs = input$rhs,
                        value = input$value,
                        free = input$free,
                        group = input$group,
                        na = input$na,
                        remove_if_fixed = input$remove_if_fixed
                    )
                )

            invisible(self)
        },

        #' @description Removes parameter with the ID remove_id.
        #' @keywords internal
        #' @param remove_id Numeric.
        remove_parameter = function(remove_id) {
            if (!(remove_id %in% private$par_table$id)) {
                warning("ParTable: Could not find id that is to be removed.")
            }

            private$par_table <-
                private$par_table |>
                filter(id != remove_id)

            invisible(self)
        },

        #' @description Adds a measurement model to the ParTable instance.
        #' @keywords internal
        #' @param variable Character. Latent variable that is measured.
        #' E.g.: \code{variable = "eta1"} or \code{variable = "pi1"}
        #' @param by Character. Manifest variable from the data set that the
        #' latent variable is measured by.
        #' E.g.: \code{by = "Y11"}
        #' @param loading Character. Loading of the manifest on the latent
        #' variable.
        #' E.g.: \code{loading = "l1"} or \code{loading = "1"}
        #' @param ... Additional arguments.
        add_measurement = function(
            variable,
            by,
            loading,
            ...
        ) {
            self$add_parameter(
                lhs   = variable,
                op    = "=~",
                rhs   = by,
                value = loading,
                ...
            )

            invisible(self)
        },

        #' @description Adds an intercept to the ParTable instance.
        #' @keywords internal
        #' @param variable Character. Latent variable that is measured.
        #' E.g.: \code{variable = "eta1"} or \code{variable = "pi1"}
        #' @param intercept Character. Intercept of the variable.
        #' E.g.: \code{intercept = "i1"} or \code{intercept = "0"}
        #' @param ... Additional arguments.
        add_intercept = function(
            variable,
            intercept,
            ...
        ) {
            self$add_parameter(
                lhs   = variable,
                op    = "~1",
                rhs   = NA,
                value = intercept,
                ...
            )

            invisible(self)
        },

        #' @description Adds a variance to the ParTable instance.
        #' @keywords internal
        #' @param variable1 Character. Name of the first variable.
        #' E.g.: \code{variable1 = "eta1"} or \code{variable1 = "Y11"}
        #' @param variable2 Character. Name of the second variable.
        #' E.g.: \code{variable2 = "eta1"} or \code{variable2 = "pi1"}
        #' @param variance Character. Variance between variable1 and variable2.
        #' E.g.: \code{variance = "cov12"} or \code{variance = "0"}
        #' @param ... Additional arguments.
        add_variance = function(
            variable1,
            variable2,
            variance,
            ...
        ) {
            self$add_parameter(
                lhs   = variable1,
                op    = "~~",
                rhs   = variable2,
                value = variance,
                ...
            )

            invisible(self)
        },

        #' @description Adds a regression to the ParTable instance.
        #' @keywords internal
        #' @param variable1 Character. Name of the first variable.
        #' E.g.: \code{variable1 = "eta1"} or \code{variable1 = "Y11"}
        #' @param variable2 Character. Name of the second variable.
        #' E.g.: \code{variable2 = "eta1"} or \code{variable2 = "pi1"}
        #' @param coefficient Character. Regression coefficient between
        #' variable1 and variable2.
        #' E.g.: \code{coefficient = "beta1"} or \code{coefficient = "0"}
        #' @param ... Additional arguments.
        add_regression = function(
            variable1,
            variable2,
            coefficient,
            ...
        ) {
            self$add_parameter(
                lhs   = variable1,
                op    = "~",
                rhs   = variable2,
                value = coefficient,
                ...
            )

            invisible(self)
        },

        #' @description Adds a label definition to the ParTable instance.
        #' @keywords internal
        #' @param label Character.
        #' @param definition Character.
        #' @param ... Additional arguments.
        add_definition = function(
            label,
            definition,
            ...
        ) {
            self$add_parameter(
                lhs   = label,
                op    = ":=",
                rhs   = definition,
                value = NA,
                group = NA,
                ...
            )

            invisible(self)
        },

        #' @description Adds a group weight definition to the ParTable instance.
        #' @keywords internal
        #' @param group_weight Character.
        #' @param group Character.
        #' @param ... Additional arguments.
        add_group_weight = function(
            group_weight,
            group,
            ...
        ) {
            self$add_parameter(
                lhs   = "group",
                op    = "%",
                rhs   = "w",
                value = group_weight,
                group = group,
                ...
            )

            invisible(self)
        },

        #' @description Adds a constraint to the ParTable instance.
        #' @keywords internal
        #' @param lhs Character. Left-hand side of the constraint.
        #' E.g.: \code{lhs = "i1 + i2"}
        #' @param rhs Character. Right-hand side of the constraint.
        #' E.g.: \code{rhs = "0"}
        #' @param group Character. Indicates the group.
        #' @param ... Additional arguments.
        add_constraint = function(
            lhs,
            rhs,
            group = NA,
            ...
        ) {
            self$add_parameter(
                lhs   = lhs,
                op    = "==",
                rhs   = rhs,
                value = NA,
                group = NA,
                ...
            )

            invisible(self)
        },

        #' @description Specifies means, variances and covariances of the latent
        #' dependent variables eta.
        #' @keywords internal
        #' @param etas Named list of character vectors of length q. Each list
        #' element (character vector) represents an eta variable. The elements
        #' of the character vector are manifest variables (i.e., indicators)
        #' from the data set that measure the corresponding eta variable. The
        #' list names correspond to the names of the eta variables.
        #' E.g.:
        #' \code{etas = list(
        #'     eta1 = c("Y11", "Y12"),
        #'     eta2 = c("Y21", "Y22"),
        #'     eta3 = c("Y31", "Y32")
        #' )}
        #' @param invariance_within Logical. Indicates if measurement invariance
        #' should be imposed across eta variables.
        #' @param invariance_between Logical. Indicates if measurement
        #' invariance should be imposed across groups.
        #' @param groups Character vector of length p. Contains unique group
        #' names of the group variable from the data set.
        #' E.g.: \code{groups = c("group1", "group2", "group3")}
        #' @param lv_scaling Character. Possible values:
        #' \code{c("effect", "referent")}. Default is "effect". Scheme to
        #' identify the latent variable scale. For \code{"effect"}: effect coding
        #' scheme freely estimates all loadings and intercepts of the indicators
        #' and constraints the average of the loadings to 1 and the average of
        #' the intercepts to 0. For \code{"referent"}: referent indicator scheme
        #' sets the first loading to 1 and the first intercept to 0.
        specify_measurement_model_eta = function(
            etas,
            invariance_within,
            invariance_between,
            groups,
            lv_scaling
        ) {

            n_groups <- length(groups)
            n_etas <- length(etas)
            n_indicators <- length(etas[[1]]) # all etas have the same length

            if (n_indicators == 1L) {
                return(invisible(self))
            }

            for (group_index in 1:n_groups) {
                group <- groups[group_index]

                for (eta_index in 1:n_etas) {
                    eta <- names(etas)[eta_index]

                    indicators <- etas[[eta_index]]

                    for (ind_index in 1:n_indicators) {
                        indicator <- indicators[ind_index]

                        if ((n_indicators == 2L && n_etas == 1L) ||
                            (ind_index == 1L && lv_scaling == "referent"))  {

                            self$add_measurement(
                                by       = indicator,
                                variable = eta,
                                free     = FALSE,
                                loading  = 1,
                                group    = group,
                                na       = TRUE
                            )

                            self$add_intercept(
                                variable  = indicator,
                                free      = FALSE,
                                intercept = 0,
                                group     = group,
                                na        = TRUE
                            )

                        } else if (ind_index == 1L && lv_scaling == "effect") {

                            self$add_measurement(
                                by       = indicator,
                                variable = eta,
                                free     = FALSE,
                                loading  = "NA",
                                group    = group,
                                na = TRUE
                            )

                            self$add_intercept(
                                variable  = indicator,
                                free      = FALSE,
                                intercept = "NA",
                                group     = group,
                                na        = TRUE
                            )
                        }

                        intercept <- paste0(
                            ".nu_y_",
                            if (invariance_within >= "strong") "" else eta_index,
                            "_",
                            ind_index,
                            "_",
                            if (invariance_between >= "strong") "" else group_index
                        )

                        loading <- paste0(
                            ".lambda_y_",
                            if (invariance_within >= "weak") "" else eta_index,
                            "_",
                            ind_index,
                            "_",
                            if (invariance_between >= "weak") "" else group_index
                        )

                        variance <- paste0(
                            ".epsilon_y_",
                            if (invariance_within == "strict") "" else eta_index,
                            "_",
                            ind_index,
                            "_",
                            if (invariance_between == "strict") "" else group_index
                        )

                        self$add_measurement(
                            by       = indicator,
                            variable = eta,
                            free     = if (n_indicators <= 2L & invariance_within < "weak") FALSE else TRUE,
                            loading  = loading,
                            group    = group
                        )

                        self$add_intercept(
                            variable  = indicator,
                            free      = if (n_indicators <= 2L & invariance_within == "configural") FALSE else TRUE,
                            intercept = intercept,
                            group     = group
                        )

                        self$add_variance(
                            variable1 = indicator,
                            variable2 = indicator,
                            variance = variance,
                            group = group
                        )

                    }

                    if ((n_indicators > 2L) &&
                        (lv_scaling == "effect") &&
                        (invariance_within <= "weak") &&
                        (invariance_between <= "weak")) {

                        contraint_intercepts <- paste0(
                            ".nu_y_",
                            eta_index,
                            "_",
                            1:n_indicators,
                            "_",
                            group_index
                        )
                        contraint_intercepts <- paste0(
                            contraint_intercepts,
                            collapse = " + "
                        )

                        self$add_constraint(lhs = contraint_intercepts,
                                            rhs = 0)

                    } else if ((n_indicators > 2L) &&
                               (lv_scaling == "effect") &&
                               (invariance_within <= "weak") &&
                               (invariance_between >= "strong")) {

                        contraint_intercepts <- paste0(
                            ".nu_y_",
                            eta_index,
                            "_",
                            1:n_indicators,
                            "_"
                        )

                        contraint_intercepts <- paste0(
                            contraint_intercepts,
                            collapse = " + "
                        )

                        self$add_constraint(lhs = contraint_intercepts,
                                            rhs = 0)

                    }

                    if ((n_indicators > 2L) &&
                        (lv_scaling == "effect") &&
                        (invariance_within == "configural") &&
                        (invariance_between == "configural")) {

                        constraint_loadings <- paste0(
                            ".lambda_y_",
                            eta_index,
                            "_",
                            1:n_indicators,
                            "_",
                            group_index
                        )

                        constraint_loadings <- paste0(
                            constraint_loadings,
                            collapse = " + "
                        )

                        self$add_constraint(lhs = constraint_loadings,
                                            rhs = n_indicators)

                    } else if ((n_indicators > 2L) &&
                               (lv_scaling == "effect") &&
                               (n_indicators == ind_index) &&
                               (invariance_within == "configural") &&
                               (invariance_between >= "weak")) {

                        constraint_loadings <- paste0(
                            ".lambda_y_",
                            eta_index,
                            "_",
                            1:n_indicators,
                            "_"
                        )

                        constraint_loadings <- paste0(
                            constraint_loadings,
                            collapse = " + "
                        )

                        self$add_constraint(lhs = constraint_loadings,
                                            rhs = n_indicators)
                    }
                }
            }

            if ((n_indicators > 2L) &&
                (lv_scaling == "effect") &&
                (invariance_within >= "strong") &&
                (invariance_between >= "strong")) {

                contraint_intercepts <- paste0(
                    ".nu_y_",
                    "_",
                    1:n_indicators,
                    "_"
                )
                contraint_intercepts <- paste0(
                    contraint_intercepts,
                    collapse = " + "
                )

                self$add_constraint(lhs = contraint_intercepts,
                                    rhs = 0)

            }

            if ((n_indicators > 2L) &&
                (lv_scaling == "effect") &&
                (invariance_within >= "weak") &&
                (invariance_between >= "weak")) {

                constraint_loadings <- paste0(
                    ".lambda_y_",
                    "_",
                    1:n_indicators,
                    "_"
                )
                constraint_loadings <- paste0(
                    constraint_loadings,
                    collapse = " + "
                )
                self$add_constraint(lhs = constraint_loadings,
                                    rhs = n_indicators)
            }

            invisible(self)
        },

        #' @description Specifies residual covariances across indicators.
        #' @keywords internal
        #' @param etas Named list of character vectors of length q. Each list
        #' element (character vector) represents an eta variable. The elements
        #' of the character vector are manifest variables (i.e., indicators)
        #' from the data set that measure the corresponding eta variable. The
        #' list names correspond to the names of the eta variables.
        #' E.g.:
        #' \code{etas = list(
        #'     eta1 = c("Y11", "Y12"),
        #'     eta2 = c("Y21", "Y22"),
        #'     eta3 = c("Y31", "Y32")
        #' )}
        #' @param resid_cov List of Characters.
        #' @param equal_resid_cov List of Characters.
        #' @param groups Character vector of length p. Contains unique group
        #' names of the group variable from the data set.
        #' E.g.: \code{groups = c("group1", "group2", "group3")}
        specify_resid_cov = function(
            etas,
            groups,
            resid_cov,
            equal_resid_cov
        ) {
            n_etas <- length(etas)
            indicators <- unlist(etas)
            n_indicators <- length(indicators)
            n_groups <- length(groups)

            # UNequal residual covariances

            if (length(resid_cov) > 0L) {
                for (ind_index1 in 1:(n_indicators - 1)) {
                    indicator1 <- indicators[ind_index1]

                    for (ind_index2 in (ind_index1 + 1):n_indicators) {
                        indicator2 <- indicators[ind_index2]

                        constraint_ind1 <-
                            which(sapply(resid_cov, function(x) indicator1 %in% x))
                        constraint_ind2 <-
                            which(sapply(resid_cov, function(x) indicator2 %in% x))

                        if (length(constraint_ind1) > 0L &&
                            length(constraint_ind2) > 0L &&
                            constraint_ind1 == constraint_ind2) {

                            for (group_index in 1:n_groups) {
                                group <- groups[group_index]

                                self$add_variance(
                                    variable1 = indicator1,
                                    variable2 = indicator2,
                                    variance = paste0(
                                        ".epsilon_y_",
                                        ind_index1,
                                        "_",
                                        ind_index2,
                                        "_",
                                        group_index
                                    ),
                                    group = group
                                )
                            }
                        }
                    }
                }
            }

            # EQUAL residual covariances

            if (length(equal_resid_cov) > 0L) {
                for (ind_index1 in 1:(n_indicators - 1)) {
                    indicator1 <- indicators[ind_index1]

                    for (ind_index2 in (ind_index1 + 1):n_indicators) {
                        indicator2 <- indicators[ind_index2]

                        constraint_ind1 <-
                            which(sapply(equal_resid_cov, function(x) indicator1 %in% x))
                        constraint_ind2 <-
                            which(sapply(equal_resid_cov, function(x) indicator2 %in% x))

                        if (length(constraint_ind1) > 0L &&
                            length(constraint_ind2) > 0L &&
                            constraint_ind1 == constraint_ind2) {

                            for (group_index in 1:n_groups) {
                                group <- groups[group_index]

                                self$add_variance(
                                    variable1 = indicator1,
                                    variable2 = indicator2,
                                    variance = paste0(
                                        ".epsilon_y_",
                                        constraint_ind1,
                                        "_",
                                        "_",
                                        group_index
                                    ),
                                    group = group
                                )
                            }
                        }
                    }
                }
            }
        },


        #' @description Specifies means, variances and covariances of the latent
        #' dependent variables eta.
        #' @keywords internal
        #' @param covariates Named list of character vectors. Each list
        #' element (character vector) represents a covariate. The elements
        #' of the character vector are manifest variables (i.e., indicators)
        #' from the data set that measure the corresponding covariate. The list
        #' names correspond to the names of the covariates.
        #' E.g.:
        #' \code{covariates = list(
        #'     covariate1 = c("cov11", "cov12"),
        #'     covariate2 = c("cov21", "cov22"),
        #'     covariate3 = c("cov31", "cov32")
        #' )}
        #' @param pis Character vector of length q. Elements are the names of
        #' the latent contrast variables pi.
        #' E.g.: {pis = c("pi1", "pi2", "pi3")}
        #' @param invariance_between Logical. Indicates if measurement
        #' invariance should be imposed across groups.
        #' @param groups Character vector of length p. Contains unique group
        #' names of the group variable from the data set.
        #' E.g.: \code{groups = c("group1", "group2", "group3")}
        #' @param lv_scaling Character. Possible values:
        #' @param fixed_covariates Logical. Indicates whether manifest
        #' covariates should be treated as fixed.
        #' @param covariates_means tibble(). Contains a tibble() with three
        #' columns: (1) .group: group names; (2) covariate: covariate names;
        #' (3) mean: mean of covariate c in group g.
        #' \code{c("effect", "referent")}. Default is "effect". Scheme to
        #' identify the latent variable scale. For \code{"effect"}: effect coding
        #' scheme freely estimates all loadings and intercepts of the indicators
        #' and constraints the average of the loadings to 1 and the average of
        #' the intercepts to 0. For \code{"referent"}: referent indicator scheme
        #' sets the first loading to 1 and the first intercept to 0.
        specify_measurement_model_covariates = function(
            covariates,
            pis,
            invariance_between,
            groups,
            lv_scaling
        ) {

            n_groups <- length(groups)
            n_covariates <- length(covariates)

            if (n_covariates == 0L) {
                return(invisible(self))
            }

            for (group_index in 1:n_groups) {
                group <- groups[group_index]

                for (covariate_index in 1:n_covariates) {
                    covariate <- names(covariates)[covariate_index]

                    n_indicators <- length(covariates[[covariate_index]])

                    if (n_indicators > 1L) {
                        self$add_intercept(
                            variable  = covariate,
                            free      = TRUE,
                            intercept = paste0(".alpha_cov_", covariate_index, "_", "_", group_index),
                            group     = group
                        )

                        self$add_variance(
                            variable1 = covariate,
                            variable2 = covariate,
                            variance  = paste0(
                                ".sigma_cov_",
                                covariate_index,
                                "_",
                                "_",
                                group_index
                            ),
                            group = group
                        )
                    } else {
                        self$add_intercept(
                            variable  = covariate,
                            free      = FALSE,
                            intercept = paste0(".nu_cov_", covariate_index, "_", "_", group_index),
                            group     = group,
                            remove_if_fixed = TRUE
                        )

                        self$add_variance(
                            variable1 = covariate,
                            variable2 = covariate,
                            variance  = paste0(
                                ".epsilon_cov_",
                                covariate_index,
                                "_",
                                "_",
                                group_index
                            ),
                            free = FALSE,
                            group = group,
                            remove_if_fixed = TRUE
                        )
                    }

                    for (pi_index in 1:length(pis)) {
                        pi <- pis[pi_index]

                        self$add_regression(
                            variable1  = pi,
                            variable2  = covariate,
                            coefficient = paste0(".beta_", pi_index, "_", covariate_index, "_", group_index),
                            group     = group
                        )
                    }

                    indicators <- covariates[[covariate_index]]

                    if (n_indicators > 1L) {
                        for (ind_index in 1:n_indicators) {
                            indicator <- indicators[ind_index]

                            if ((n_indicators == 2L) ||
                                (ind_index == 1L) &&
                                (lv_scaling == "referent")) {

                                self$add_measurement(
                                    by       = indicator,
                                    variable = covariate,
                                    free     = FALSE,
                                    loading  = 1,
                                    group    = group,
                                    na       = TRUE
                                )

                                self$add_intercept(
                                    variable  = indicator,
                                    free      = FALSE,
                                    intercept = 0,
                                    group     = group,
                                    na        = TRUE
                                )

                            } else if ((ind_index == 1L) &&
                                       (lv_scaling == "effect")) {

                                self$add_measurement(
                                    by       = indicator,
                                    variable = covariate,
                                    free     = FALSE,
                                    loading  = "NA",
                                    group    = group,
                                    na = TRUE
                                )
                            }

                            intercept <- paste0(
                                ".nu_cov_",
                                covariate_index,
                                "_",
                                ind_index,
                                "_",
                                if (invariance_between >= "strong") "" else group_index
                            )

                            loading <- paste0(
                                ".lambda_cov_",
                                covariate_index,
                                "_",
                                ind_index,
                                "_",
                                if (invariance_between >= "weak") "" else group_index
                            )

                            variance <- paste0(
                                ".epsilon_cov_",
                                covariate_index,
                                "_",
                                ind_index,
                                "_",
                                if (invariance_between == "strict") "" else group_index
                            )

                            self$add_measurement(
                                by       = indicator,
                                variable = covariate,
                                free     = if (n_indicators < 3L) FALSE else TRUE,
                                loading  = loading,
                                group    = group
                            )

                            self$add_intercept(
                                variable  = indicator,
                                free      = if (n_indicators < 3L) FALSE else TRUE,
                                intercept = intercept,
                                group     = group
                            )

                            self$add_variance(
                                variable1 = indicator,
                                variable2 = indicator,
                                variance = variance,
                                group = group
                            )

                        }

                        if ((n_indicators > 2L) &&
                            (lv_scaling == "effect") &&
                            (invariance_between == "configural")) {

                            constraint_loadings <- paste0(
                                ".lambda_cov_",
                                covariate_index,
                                "_",
                                1:n_indicators,
                                "_",
                                group_index
                            )

                            constraint_loadings <- paste0(
                                constraint_loadings,
                                collapse = " + "
                            )

                            contraint_intercepts <- paste0(
                                ".nu_cov_",
                                covariate_index,
                                "_",
                                1:n_indicators,
                                "_",
                                group_index
                            )
                            contraint_intercepts <- paste0(
                                contraint_intercepts,
                                collapse = " + "
                            )

                            self$add_constraint(lhs = constraint_loadings,
                                                rhs = n_indicators)
                            self$add_constraint(lhs = contraint_intercepts,
                                                rhs = 0)

                        } else if ((n_indicators > 2L) &&
                                   (lv_scaling == "effect") &&
                                   (invariance_between == "weak" && group_index < n_groups)) {

                            contraint_intercepts <- paste0(
                                ".nu_cov_",
                                covariate_index,
                                "_",
                                1:n_indicators,
                                "_",
                                group_index
                            )

                            contraint_intercepts <- paste0(
                                contraint_intercepts,
                                collapse = " + "
                            )

                            self$add_constraint(lhs = contraint_intercepts,
                                                rhs = 0)
                        } else if ((n_indicators > 2L) &&
                                   (lv_scaling == "effect") &&
                                   (invariance_between == "weak" && group_index == n_groups)) {

                            contraint_intercepts <- paste0(
                                ".nu_cov_",
                                covariate_index,
                                "_",
                                1:n_indicators,
                                "_",
                                group_index
                            )

                            contraint_intercepts <- paste0(
                                contraint_intercepts,
                                collapse = " + "
                            )

                            self$add_constraint(lhs = contraint_intercepts,
                                                rhs = 0)

                            constraint_loadings <- paste0(
                                ".lambda_cov_",
                                covariate_index,
                                "_",
                                1:n_indicators,
                                "_"
                            )

                            constraint_loadings <- paste0(
                                constraint_loadings,
                                collapse = " + "
                            )

                            self$add_constraint(lhs = constraint_loadings,
                                                rhs = n_indicators)
                        } else if ((n_indicators > 2L) &&
                                   (lv_scaling == "effect") &&
                                   (invariance_between == "strong" && group_index == n_groups)) {

                            constraint_loadings <- paste0(
                                ".lambda_cov_",
                                covariate_index,
                                "_",
                                1:n_indicators,
                                "_"
                            )

                            constraint_loadings <- paste0(
                                constraint_loadings,
                                collapse = " + "
                            )

                            contraint_intercepts <- paste0(
                                ".nu_cov_",
                                covariate_index,
                                "_",
                                1:n_indicators,
                                "_"
                            )
                            contraint_intercepts <- paste0(
                                contraint_intercepts,
                                collapse = " + "
                            )

                            self$add_constraint(lhs = constraint_loadings,
                                                rhs = n_indicators)
                            self$add_constraint(lhs = contraint_intercepts,
                                                rhs = 0)

                        }
                    }
                }
            }

            invisible(self)
        },


        #' @description Specifies means, variances and covariances of the latent
        #' dependent variables eta.
        #' @keywords internal
        #' @param etas Named list of character vectors of length q. Each list
        #' element (character vector) represents an eta variable. The elements
        #' of the character vector are manifest variables (i.e., indicators)
        #' from the data set that measure the corresponding eta variable. The
        #' list names correspond to the names of the eta variables.
        #' E.g.:
        #' \code{etas = list(
        #'     eta1 = c("Y11", "Y12"),
        #'     eta2 = c("Y21", "Y22"),
        #'     eta3 = c("Y31", "Y32")
        #' )}
        #' @param groups Character vector of length p. Contains unique group
        #' names of the group variable from the data set.
        #' E.g.: \code{groups = c("group1", "group2", "group3")}
        #' @param compound_symmetry Logical. Indicates whether compound symmetry
        #' should be imposed.
        #' @param variance_homogeneity Logical. Default is \code{TRUE}.
        #' Indicates whether homogeneity of variance should be imposed across
        #' groups.
        specify_etas = function(
            etas,
            groups,
            compound_symmetry,
            variance_homogeneity
        ) {
            # intercepts eta

            for (eta_index in 1:length(etas)) {

                eta <- names(etas)[eta_index]

                for (group_index in 1:length(groups)) {
                    group <- groups[group_index]

                    self$add_intercept(
                        variable = eta,
                        intercept = 0,
                        free = FALSE,
                        group = group
                    )

                    self$add_intercept(
                        variable = eta,
                        intercept = paste0(".alpha_eta_", eta_index, "_", group_index),
                        free = FALSE,
                        group = group,
                        na = TRUE
                    )
                }
            }

            for (eta_index1 in 1:length(etas)) {
                eta1 <- names(etas)[eta_index1]

                for (eta_index2 in eta_index1:length(etas)) {
                    eta2 <- names(etas)[eta_index2]

                    for (group_index in 1:length(groups)) {
                        group <- groups[group_index]

                        if (compound_symmetry) {

                            if (eta_index1 == eta_index2) {
                                self$add_variance(
                                    variable1 = eta1,
                                    variable2 = eta2,
                                    variance = paste0(
                                        ".sigma_eta_", "i", "_", "i", "_",
                                        if (variance_homogeneity) "" else group_index
                                    ),
                                    group = group
                                )
                            } else {
                                self$add_variance(
                                    variable1 = eta1,
                                    variable2 = eta2,
                                    variance = paste0(
                                        ".sigma_eta_", "i", "_", "j", "_",
                                        if (variance_homogeneity) "" else group_index
                                    ),
                                    group = group
                                )
                            }

                        } else {
                            self$add_variance(
                                variable1 = eta1,
                                variable2 = eta2,
                                variance = 0,
                                free = FALSE,
                                group = group
                            )
                        }

                    }
                }
            }

            invisible(self)
        },

        #' @description Specifies the measurement model pi =~ eta, means,
        #' variances and covariances of the latent contrast variables pi.
        #' @keywords internal
        #' @param etas Named list of character vectors of length q. Each list
        #' element (character vector) represents an eta variable. The elements
        #' of the character vector are manifest variables (i.e., indicators)
        #' from the data set that measure the corresponding eta variable. The
        #' list names correspond to the names of the eta variables.
        #' E.g.:
        #' \code{etas = list(
        #'     eta1 = c("Y11", "Y12"),
        #'     eta2 = c("Y21", "Y22"),
        #'     eta3 = c("Y31", "Y32")
        #' )}
        #' @param pis Character vector of length q. Elements are the names of
        #' the latent contrast variables pi.
        #' E.g.: {pis = c("pi1", "pi2", "pi3")}
        #' @param c_matrix_within Numeric q x q matrix. Contrast matrix for the
        #' within-subjects design. Must be a square matrix.
        #' E.g.:
        #' \code{
        #' c_matrix_within = matrix(c(
        #'     1/3, -sqrt(1/2),  sqrt(1/6),
        #'     1/3,          0, -sqrt(2/3),
        #'     1/3,  sqrt(1/2),  sqrt(1/6)
        #' ), nrow = 3)
        #' }
        #' @param groups Character vector of length p. Contains unique group
        #' names of the group variable from the data set.
        #' E.g.: \code{groups = c("group1", "group2", "group3")}
        #' @param sphericity List of character vectors. Each character vector
        #' contains indices of pi variables among which sphericity should be
        #' imposed.
        #' @param compound_symmetry Logical. Indicates whether compound symmetry
        #' should be imposed.
        #' @param variance_homogeneity Logical. Default is \code{TRUE}.
        #' Indicates whether homogeneity of variance should be imposed across
        #' groups.
        specify_pis = function(
            etas,
            pis,
            c_matrix_within,
            groups,
            sphericity,
            compound_symmetry,
            variance_homogeneity
        ) {

            b_matrix_within <- solve(c_matrix_within)

            # measurement model
            for (pi_index in 1:ncol(b_matrix_within)) {
                pi <- pis[pi_index]
                for (eta_index in 1:length(etas)){
                    eta <- names(etas)[eta_index]
                    for (group_index in 1:length(groups)) {
                        group <- groups[group_index]

                        self$add_measurement(
                            variable = pi,
                            by       = eta,
                            free     = 0,
                            loading  = b_matrix_within[eta_index,pi_index],
                            group    = group
                        )
                    }
                }
            }

            # intercepts
            for (pi_index in 1:length(pis)) {
                pi <- pis[pi_index]
                for (group_index in 1:length(groups)) {
                    group <- groups[group_index]

                    self$add_intercept(
                        variable = pi,
                        free  = TRUE,
                        intercept = paste0(".alpha_pi_", pi_index, "_", group_index),
                        group = group
                    )
                }
            }

            # variances
            for (pi_index1 in 1:length(pis)) {
                pi1 <- pis[pi_index1]

                for (pi_index2 in pi_index1:length(pis)) {
                    pi2 <- pis[pi_index2]

                    for (group_index in 1:length(groups)) {
                        group <- groups[group_index]

                        if (compound_symmetry) {
                            self$add_variance(
                                variable1 = pi1,
                                variable2 = pi2,
                                variance = 0,
                                group = group
                            )
                        } else if (length(sphericity) == 0L) {
                            self$add_variance(
                                variable1 = pi1,
                                variable2 = pi2,
                                variance = paste0(
                                    ".sigma_pi_",
                                    pi_index1, "_", pi_index2, "_",
                                    if (variance_homogeneity) "" else group_index
                                ),
                                group = group
                            )
                        } else if (length(sphericity) > 0L) {
                            constraint_pi1 <- which(sapply(sphericity, function(x) pi_index1 %in% x))
                            constraint_pi2 <- which(sapply(sphericity, function(x) pi_index2 %in% x))

                            if (length(constraint_pi1) == 0L || length(constraint_pi2) == 0L) {

                                if (pi_index1 == pi_index2) {
                                    self$add_variance(
                                        variable1 = pi1,
                                        variable2 = pi2,
                                        variance = paste0(
                                            ".sigma_pi_", pi_index1, "_", pi_index2, "_",
                                            if (variance_homogeneity) "" else group_index
                                        ),
                                        group = group
                                    )
                                } else {
                                    self$add_variance(
                                        variable1 = pi1,
                                        variable2 = pi2,
                                        variance = paste0(
                                            ".sigma_pi_", pi_index1, "_", pi_index2, "_",
                                            if (variance_homogeneity) "" else group_index
                                        ),
                                        group = group
                                    )
                                }
                            } else if (xor(length(constraint_pi1) == 0L, length(constraint_pi2) == 0L)) {
                                self$add_variance(
                                    variable1 = pi1,
                                    variable2 = pi2,
                                    variance = paste0(
                                        ".sigma_pi_", pi_index1, "_", pi_index2, "_",
                                        if (variance_homogeneity) "" else group_index
                                    ),
                                    group = group
                                )
                            } else if (constraint_pi1 != constraint_pi2) {
                                self$add_variance(
                                    variable1 = pi1,
                                    variable2 = pi2,
                                    variance = paste0(
                                        ".sigma_pi_", pi_index1, "_", pi_index2, "_",
                                        if (variance_homogeneity) "" else group_index
                                    ),
                                    group = group
                                )
                            } else {
                                if (pi_index1 == pi_index2) {
                                    self$add_variance(
                                        variable1 = pi1,
                                        variable2 = pi2,
                                        variance = paste0(
                                            ".sigma_pi_", constraint_pi1, "_", "_",
                                            if (variance_homogeneity) "" else group_index
                                        ),
                                        group = group
                                    )
                                } else {
                                    self$add_variance(
                                        variable1 = pi1,
                                        variable2 = pi2,
                                        variance = 0,
                                        group = group
                                    )
                                }

                            }
                        }
                    }
                }
            }

            invisible(self)
        },


        # group % c(gw00,gw01,gw10,gw11)*w
        # N := exp(gw00) + exp(gw01) + exp(gw10) + exp(gw11)
        # relfreq00 := exp(gw00)/N # Men.NotMarried
        # relfreq01 := exp(gw01)/N # Men.Married
        # relfreq10 := exp(gw10)/N # Women.NotMarried
        # relfreq11 := exp(gw11)/N # Women.Married


        #' @description Specifies the group weights.
        #' @keywords internal
        #' @param groups Character vector of length p. Contains unique group
        #' names of the group variable from the data set.
        #' E.g.: \code{groups = c("group1", "group2", "group3")}
        #' @param group_weights Either numeric vector of length p (same length
        #' as \code{group_labels}) or one of the characters
        specify_group_weights = function(
            groups,
            group_weights
        ) {

            if (is.character(group_weights) && group_weights == "fixed") {
                group_weights <- rep_len(1/length(groups), length(groups))
            }

            rel_freqs <- paste0("relfreq", 1:length(groups))

            if (is.character(group_weights) && group_weights == "stochastic") {
                group_weights <- paste0("gw", 1:length(groups))

                self$add_definition(
                    label = "N",
                    definition   = paste0("exp(", group_weights, ")") |>
                        paste0(collapse = " + ")
                )

                for (group_index in 1:length(groups)) {
                    group <- groups[group_index]
                    group_weight <- group_weights[group_index]
                    rel_freq <- rel_freqs[group_index]

                    self$add_group_weight(
                        group_weight = group_weight,
                        group = group
                    )

                    self$add_definition(
                        label      = rel_freq,
                        definition = paste0("exp(", group_weight, ") / N")
                    )
                }


            } else if (is.numeric(group_weights)) {

                for (group_index in 1:length(groups)) {
                    group_weight <- group_weights[group_index]
                    rel_freq <- rel_freqs[group_index]

                    self$add_definition(
                        label      = rel_freq,
                        definition = group_weight
                    )
                }
            }

            invisible(self)
        },


        #' @description Returns the model specification as a lavaan model
        #' syntax.
        #' @param fixed_covariates Logical. For internal purposes.
        #' @param remove_unfree Logical. For internal purposes.
        #' @param remove_na Logical. For internal purposes.
        #' @param remove_constraints Logical. For internal purposes.
        #' @param remove_definitions Logical. For internal purposes.
        #' @param first_group_only Logical. For internal purposes.
        #' @keywords internal
        get_lav_syntax = function(
            fixed_covariates = FALSE,
            remove_unfree = FALSE,
            remove_na = FALSE,
            remove_constraints = FALSE,
            remove_definitions = FALSE,
            select_group = NULL,
            break_rhs = TRUE
        ) {

            if (break_rhs) {
                rhs_sep <- " +\n    "
            } else {
                rhs_sep <- " + "
            }

            if (is.null(select_group)) {
                select_group <- unique(private$par_table$group)
            }

            private$par_table |>
                filter(group %in% select_group) |>
                filter(!remove_constraints | op != "==") |>
                filter(!remove_definitions | op != ":=") |>
                filter(!remove_na | na == FALSE) |>
                filter(!remove_unfree | free) |>
                filter(fixed_covariates | !remove_if_fixed) |>
                group_by(op, lhs) |>
                group_map(function(tbl, desc) {
                    lhs <- desc$lhs
                    if (desc$op == "~1") {
                        op <- "~"
                        tbl$rhs <- "1"
                    } else {
                        op <- desc$op
                    }

                    rhss <- tbl |>
                        group_by(rhs, na) |>
                        group_map(function(tbl, desc) {
                            rhs <- desc$rhs
                            loadings <- tbl$value |>
                                paste0(collapse = ", ")

                            if (op %in% c(":=", "==")) {
                                desc$rhs
                            } else if (nrow(tbl) == 1L) {
                                paste0(loadings, "*", desc$rhs)
                            } else {
                                paste0("c(", loadings, ")*", desc$rhs)
                            }
                        }) |> paste0(collapse = rhs_sep)
                    paste0(lhs, " ", op, "\n    ", rhss)
                }) |> paste0(collapse = "\n")
        },

        #' @description Returns an array of parameter labels used in the lavaan syntax.
        #' @param pis Character vector of length q. Elements are the names of
        #' the latent contrast variables pi.
        #' E.g.: {pis = c("pi1", "pi2", "pi3")}
        #' @param covariates Named list of character vectors. Each list
        #' element (character vector) represents a covariate. The elements
        #' of the character vector are manifest variables (i.e., indicators)
        #' from the data set that measure the corresponding covariate. The list
        #' names correspond to the names of the covariates.
        #' E.g.:
        #' \code{covariates = list(
        #'     covariate1 = c("cov11", "cov12"),
        #'     covariate2 = c("cov21", "cov22"),
        #'     covariate3 = c("cov31", "cov32")
        #' )}
        #' @param groups Character vector of length p. Contains unique group
        #' names of the group variable from the data set.
        #' E.g.: \code{groups = c("group1", "group2", "group3")}
        get_par_labels = function(pis, covariates, groups) {

            n_covariates <- length(covariates)
            n_pis <- length(pis)
            n_groups <- length(groups)

            labels <- character()

            for (covariate_index in 0:n_covariates) {
                for (pi_index in 1:n_pis) {
                    for (group_index in 1:n_groups) {
                        if (covariate_index == 0L) {
                            labels <- c(labels, paste0(".alpha_pi_", pi_index, "_", group_index))
                        } else {
                            labels <- c(labels, paste0(".beta_", pi_index, "_", covariate_index, "_", group_index))
                        }
                    }
                }
            }

            labels <- array(labels, dim = c(n_groups, n_pis, n_covariates + 1))

            labels
        },
        get_par_labels2 = function(pis, covariates, groups) {

            n_covariates <- length(covariates)
            n_pis <- length(pis)
            n_groups <- length(groups)

            labels <- array("", dim = c(n_covariates + 1, n_pis, n_groups))

            for (covariate_index in 0:n_covariates) {
                for (pi_index in 1:n_pis) {
                    for (group_index in 1:n_groups) {
                        if (covariate_index == 0L) {
                            labels[covariate_index + 1, pi_index, group_index] <-
                                paste0(".alpha_pi_", pi_index, "_", group_index)
                        } else {
                            labels[covariate_index + 1, pi_index, group_index] <-
                                paste0(".beta_", pi_index, "_", covariate_index, "_", group_index)
                        }
                    }
                }
            }

            # Add dimnames
            dimnames(labels) <- list(
                covariate = c("intercept", covariates),
                pi = pis,
                group = groups
            )

            labels
        },

        B_array_labels = function(pis, covariates, groups) {

            n_covariates <- length(covariates)
            n_pis <- length(pis)
            n_groups <- length(groups)

            labels <- array("", dim = c(n_covariates + 1, n_pis, n_groups))

            for (covariate_index in 0:n_covariates) {
                for (pi_index in 1:n_pis) {
                    for (group_index in 1:n_groups) {
                        if (covariate_index == 0L) {
                            labels[covariate_index + 1, pi_index, group_index] <-
                                paste0(".alpha_pi_", pi_index, "_", group_index)
                        } else {
                            labels[covariate_index + 1, pi_index, group_index] <-
                                paste0(".beta_", pi_index, "_", covariate_index, "_", group_index)
                        }
                    }
                }
            }

            # Add dimnames
            dimnames(labels) <- list(
                covariate = c("intercept", covariates),
                pi = pis,
                group = groups
            )

            labels
        },

        B_vec_type = function(pis, covariates, groups) {
            n_covariates <- length(covariates)
            n_pis <- length(pis)
            n_groups <- length(groups)

            labels <- array("", dim = c(n_covariates + 1, n_pis, n_groups))

            for (covariate_index in 0:n_covariates) {
                for (pi_index in 1:n_pis) {
                    for (group_index in 1:n_groups) {
                        if (covariate_index == 0L) {
                            labels[covariate_index + 1, pi_index, group_index] <- "pi"
                        } else {
                            labels[covariate_index + 1, pi_index, group_index] <- "covariate"
                        }
                    }
                }
            }

            labels
        },

        B_vec_picov_indices = function(pis, covariates, groups) {
            n_covariates <- length(covariates)
            n_pis <- length(pis)
            n_groups <- length(groups)

            labels <- array("", dim = c(n_covariates + 1, n_pis, n_groups))

            for (covariate_index in 0:n_covariates) {
                for (pi_index in 1:n_pis) {
                    for (group_index in 1:n_groups) {
                        if (covariate_index == 0L) {
                            labels[covariate_index + 1, pi_index, group_index] <- pi_index
                        } else {
                            labels[covariate_index + 1, pi_index, group_index] <- covariate_index
                        }
                    }
                }
            }

            labels
        },

        B_vec_group_indices = function(pis, covariates, groups) {
            n_covariates <- length(covariates)
            n_pis <- length(pis)
            n_groups <- length(groups)

            labels <- array("", dim = c(n_covariates + 1, n_pis, n_groups))

            for (covariate_index in 0:n_covariates) {
                for (pi_index in 1:n_pis) {
                    for (group_index in 1:n_groups) {
                        labels[covariate_index + 1, pi_index, group_index] <- group_index
                    }
                }
            }

            labels
        },

        #' @importFrom stringr str_glue
        B_var_vec_labels = function(pis, covariates, groups) {
            types <- as.vector(self$B_vec_type(
                pis = pis,
                covariates = covariates,
                groups = groups
            ))
            picov_indices <- as.vector(self$B_vec_picov_indices(
                pis = pis,
                covariates = covariates,
                groups = groups
            ))
            group_indices <- as.vector(self$B_vec_group_indices(
                pis = pis,
                covariates = covariates,
                groups = groups
            ))

            n <- length(types)
            vcov_vec_labels <- matrix("", nrow = n, ncol = n)
            names_vec <- character(n)

            for (i in 1:n) {
                # Assign dimname label to names_vec
                names_vec[i] <- stringr::str_glue(
                    ".{ifelse(types[i] == 'pi', 'pi', 'cov')}_{picov_indices[i]}_{group_indices[i]}"
                )

                for (j in i:n) {
                    if (types[i] != "pi" || types[j] != "pi" || group_indices[i] != group_indices[j]) {
                        vcov_vec_labels[i, j] <- vcov_vec_labels[j, i] <- "0"
                    } else {
                        vcov_vec_labels[i, j] <- vcov_vec_labels[j, i] <-
                            str_glue(".sigma_pi_{picov_indices[i]}_{picov_indices[j]}_{group_indices[i]}")
                    }
                }
            }

            # Assign symmetric dimnames
            dimnames(vcov_vec_labels) <- list(row = names_vec, col = names_vec)

            return(vcov_vec_labels)
        }

    )
)

