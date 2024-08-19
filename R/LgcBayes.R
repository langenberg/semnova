#' LgcBayes Class
#'
#' @description Some description
#' @export
#' @rdname lgc
#' @importFrom R6 R6Class
#' @import tidyverse
#' @details \code{lgc_bayes()} is a wrapper for the R6 class constructor
#' \code{LgcBayes$new()}. See \code{Lgc$new()} below for a list of arguments.
#'
LgcBayes <- R6Class(
    "LgcBayes",
    private = list(
        etas                 = character(),
        etas_mmodel          = list(),
        resid_cov            = list(),
        equal_resid_cov      = list(),
        invariance_within    = "strong",
        c_matrix_within      = matrix(),
        compound_symmetry    = FALSE,
        pis                  = character(),
        sphericity           = FALSE,
        group                = character(),
        group_labels         = character(),
        group_weights        = "fixed",
        variance_homogeneity = FALSE,
        covariates           = character(),
        covariates_mmodel    = list(),
        fixed_covariates     = TRUE,
        invariance_between   = "strong",
        c_matrix_between     = matrix(),
        lv_scaling           = "effect",
        par_table            = NULL,
        options              = NULL,
        data                 = NULL,
        hypotheses           = list(),
        sem_obj              = NULL,
        append               = character(),
        input_lgc            = list(),
        dummy                = FALSE,

        # #' @keywords internal
        # deep_clone = function(name, value) {
        #     if (name == "hypotheses") {
        #         lapply(value, function(hypothesis) hypothesis$clone())
        #     } else {
        #         value
        #     }
        # },

        #' @keywords internal
        #' @import tidyverse
        is_latent = function(variables) {
            is_latent_inner <- function(variable) {
                if (variable %in% names(private$etas_mmodel)) {
                    max(sapply(private$etas_mmodel, length)) > 1L
                } else if (str_detect(variable, "^\\.pi")) {
                    TRUE
                } else {
                    if (!is.null(private$covariates_mmodel) && !(length(private$covariates_mmodel) == 0L)) {

                        manifest <- any(sapply(private$covariates_mmodel, function(indicators) variable %in% indicators))

                        if (manifest) {
                            FALSE
                        } else if (variable %in% names(private$covariates_mmodel)) {
                            TRUE
                        } else {
                            FALSE
                        }
                    } else {
                        FALSE
                    }
                }
            }

            sapply(variables, is_latent_inner)
        },

        #' @keywords internal
        has_latent_dv = function() {
            any(sapply(private$etas_mmodel, length) > 1L)
        },

        #' @keywords internal
        has_latent_covariate = function() {
            any(sapply(private$covariates_mmodel, length) > 1L)
        },

        #' @keywords internal
        has_covariate = function() {
            length(private$covariates_mmodel) > 0L
        },

        #' @keywords internal
        has_within = function() {
            prod(sapply(private$withins, length)) > 1L
        },

        #' @keywords internal
        has_resid_cov = function() {
            length(private$resid_cov) > 0L
        },

        #' @keywords internal
        #' @import tidyverse
        is_indicator = function(variables) {
            is_indicator_inner <- function(variable) {
                if (variable %in% unlist(private$etas_mmodel)) {
                    max(sapply(private$etas_mmodel, length)) > 1L
                } else if (str_detect(variable, "^\\.pi")) {
                    FALSE
                } else {
                    if (!is.null(private$covariates_mmodel) && !(length(private$covariates_mmodel) == 0L)) {

                        manifest <- which(sapply(private$covariates_mmodel, function(indicators) variable %in% indicators))

                        if (length(manifest) == 0L) {
                            FALSE
                        } else {
                            length(private$covariates_mmodel[[manifest]]) > 1L
                        }
                    } else {
                        NA
                    }
                }
            }

            sapply(variables, is_indicator_inner)
        },

        #' @keywords internal
        #' @import tidyverse
        belongs_to = function(variables, whom) {
            belongs_to_inner <- function(variable) {
                if (whom == "covariate") {
                    variable %in% unlist(private$covariates_mmodel)
                } else if (whom == "eta") {
                    variable %in% unlist(private$etas_mmodel) || str_detect(variable, "^\\.eta")
                } else if (whom == "pi") {
                    str_detect(variable, "^\\.pi")
                } else {
                    FALSE
                }
            }

            sapply(variables, belongs_to_inner)
        },

        #' @keywords internal
        #' @import tidyverse
        match_group = function(group, return_group) {

            idata <- expand.grid("group" = private$group_labels)
            names(idata) <- private$group

            group_indices <- sapply(group, function(g) which(g == private$group_labels))
            groups <- idata[group_indices,,drop = F]
            groups <- lapply(1:ncol(groups), function(x) paste0(groups[,x]))
            names(groups) <- colnames(idata)

            groups[[return_group]]
        },

        #' @keywords internal#'
        #' @import tidyverse
        get_facet_formula = function() {
            cols <- private$group

            cols <- sapply(cols, function(col)
                paste0('private$match_group(group, "', col, '")')) %>%
                paste0(collapse = " + ")

            form <- paste0(". ~", cols)
            as.formula(form)
        },

        #' @keywords internal
        #' @import tidyverse
        get_facet_grid = function() {
            form <- private$get_facet_formula()

            facet_grid(form)
        },

        #' @keywords internal
        #' @importFrom vctrs vec_as_names
        get_data_name = function(what, ..., data = NULL) {
            dots <- list(...)
            switch (what,
                    group = c(".group", names(data)),
                    data_names = c(unlist(dots, use.names = FALSE), names(data)),
                    NULL
            )
        },

        #' @keywords internal
        #' @import tidyverse
        validate = function(
        data = NULL,
        etas,
        c_matrix_within,
        resid_cov = list(),
        equal_resid_cov = list(),
        invariance_within = "strong",
        compound_symmetry = FALSE,
        sphericity = list(),
        c_matrix_between = TRUE,
        invariance_between = "strong",
        covariates = NULL,
        fixed_covariates = TRUE,
        lv_scaling = "effect",
        group = NULL,
        group_labels = NULL,
        group_weights = "fixed",
        variance_homogeneity = FALSE,
        hypotheses = NULL,
        append = NULL,
        dummy = FALSE
        ) {

            # validate: 1. etas ------------------------------------------------

            if (is.character(etas)) {
                # etas is a character vector (manifest variables)
                etas <- list(etas)
            }

            if (is.list(etas)) {
                # etas is a list

                n_ind_per_eta <- sapply(etas, length)
                n_ind_eta <- first(n_ind_per_eta)

                if (!all(sapply(n_ind_per_eta, function(x) x == n_ind_eta))) {
                    stop("Number of indicators must be equal across eta variables.")
                }

                if (n_ind_eta == 1L) {
                    names(etas) <- unlist(etas)
                } else {
                    if (is.null(names(etas))) {
                        names(etas) <- paste0(".eta", 1:length(etas))
                    }
                }

                private$etas_mmodel <- etas
                private$etas <- names(etas)
            }

            # validate: 2. groups ----------------------------------------------

            if (!dummy) {
                if (is.null(group)) {
                    data$.group <- "1"
                    private$group <- ".group"
                    private$group_labels <- "1"
                } else {
                    private$group <- group
                    private$group_labels <- unique(data[[group]])
                }
            } else {
                private$group <- ".group"
                private$group_labels <- group_labels
            }

            private$group_weights <- group_weights

            # validate: 3. covariates ------------------------------------------

            if (is.null(covariates)) {
                private$covariates <- character()
                private$covariates_mmodel <- list()
            } else {

                if (is.character(covariates)) {
                    covariates <- as.list(covariates)
                }

                if (is.null(names(covariates))) {
                    names(covariates) <-
                        paste0(".covariate", 1:length(covariates))
                }

                for (covariate_index in 1:length(covariates)) {
                    if (length(covariates[[covariate_index]]) == 1L) {
                        names(covariates)[covariate_index] <-
                            covariates[[covariate_index]]
                    }
                }

                private$covariates_mmodel <- covariates
                private$covariates <- names(covariates)
            }

            # validate: 4. pis -------------------------------------------------

            private$pis <- paste0(".pi", 1:length(private$etas))

            # validate: 5. data ------------------------------------------------

            private$data <- as_tibble(data)

            # validate: 6. sphericity & compound symmetry

            if (length(sphericity) > 0L & compound_symmetry) {
                warning("sphericity argument is not empty and compound_symmetry is TRUE. sphericity will be ignored")
                private$sphericity <- list()
                private$compound_symmetry <- compound_symmetry
            } else {
                private$sphericity <- sphericity
                private$compound_symmetry <- compound_symmetry
            }

            # validate: 7. hypotheses

            if (is.null(hypotheses)) {
                hypotheses <- NULL
            } else if (!is.list(hypotheses)) {
                hypotheses <- list(hypotheses)
            }

            if (is.list(hypotheses)) {
                if (!all(sapply(hypotheses, function(x) inherits(x, "Hypothesis")))) {
                    stop("hypotheses must be a list of objects of class Hypothesis")
                }
            }

            # validate: 8. invariance ------------------------------------------

            invariance_types <- c("configural", "weak", "strong", "strict")

            # within

            if (is.null(invariance_within) ||
                !is.character(invariance_within)) {
                invariance_within <- "strong"
            } else {
                invariance_within <- tolower(invariance_within[1])
            }

            if (!(invariance_within %in% invariance_types)) {
                warning("I don't understand the invariance_within argument. Falling back to strong invariance")
                invariance_within <- "strong"
            }

            private$invariance_within <- factor(
                invariance_within,
                levels = invariance_types,
                ordered = TRUE
            )

            # between

            if (is.null(invariance_between) ||
                !is.character(invariance_between)) {
                invariance_between <- "strong"
            } else {
                invariance_between <- tolower(invariance_between[1])
            }

            if (!(invariance_between %in% invariance_types)) {
                warning("I don't understand the invariance_between argument. Falling back to strong invariance")
                invariance_between <- "strong"
            }

            private$invariance_between <- factor(
                invariance_between,
                levels = invariance_types,
                ordered = TRUE
            )

            # validate: 9. everything else -------------------------------------

            private$fixed_covariates     <- fixed_covariates
            private$variance_homogeneity <- variance_homogeneity
            private$c_matrix_within      <- c_matrix_within
            private$resid_cov            <- resid_cov
            private$equal_resid_cov      <- equal_resid_cov
            private$c_matrix_between     <- c_matrix_between
            private$lv_scaling           <- lv_scaling
            private$hypotheses           <- hypotheses
            private$append               <- append
            private$dummy                <- dummy

        },

        #' @keywords internal
        is_label = function(labels) {
            greek_letters <- c(
                "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta",
                "theta", "iota", "kappa", "lambda", "mu", "nu", "xi", "omikron",
                "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"
            )

            capital_greek_letters <- c(
                "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta",
                "Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omikron",
                "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"
            )

            has_pattern <- function(label) {
                str_detect(label, "^.[a-zA-z]+(_[a-zA-Z0-9]*)+$")
            }

            extract_greek <- function(label) {
                label %>%
                    str_replace("^[\\.]*", "") %>%
                    str_replace("(_[a-zA-Z0-9]*)+$", "")
            }

            is_greek <- function(label) {
                any(sapply(greek_letters, function(x) x == label)) ||
                    any(sapply(capital_greek_letters, function(x) x == label))
            }

            is_label_internal <- function(label) {
                !is.null(label) &&
                    !is.na(label) &&
                    has_pattern(label) &&
                    (label %>% extract_greek() %>% is_greek())
            }

            sapply(labels, is_label_internal)
        },

        #' @keywords internal
        get_labels = function(
        labels,
        parse_action = c("underscore", "empty", "remove"),
        format = c("plotmath", "text", "syntax"),
        sep = ","
        ) {

            parse_action <- parse_action[1]
            format <- format[1]
            sep <- if (format == "plotmath") paste0(',"',sep,'",') else sep

            greek_letters <- c(
                "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta",
                "theta", "iota", "kappa", "lambda", "mu", "nu", "xi", "omikron",
                "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"
            )

            capital_greek_letters <- c(
                "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta",
                "Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omikron",
                "Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega"
            )

            is_greek <- function(label) {
                if (!str_detect(label, "^[\\.]*[a-zA-Z]+[0-9]*$")) {
                    return(FALSE)
                }

                label %>%
                    str_replace("^[\\.]*", "") %>%
                    str_replace("[0-9]*$", "") %>%
                    sapply(function(x) x %in% greek_letters | x %in% capital_greek_letters)
            }

            extract_greek_wo_number <- function(label) {
                label %>%
                    str_replace("^[\\.]*", "") %>%
                    str_replace("[0-9]*$", "")
            }

            extract_greek_number <- function(label) {
                greek_number <- label %>%
                    str_replace("^[\\.]*[a-zA-Z]*", "")
                if (greek_number == "") {
                    NULL
                } else {
                    as.numeric(greek_number)
                }
            }

            starts_with_greek <- function(label) {
                any(sapply(
                    greek_letters,
                    function(x) str_detect(label, paste0("^\\.", x))
                )) || any(sapply(
                    capital_greek_letters,
                    function(x) str_detect(label, paste0("^\\.", x))
                ))
            }

            extract_greek <- function(label) {
                str_split(label, "_")[[1]][1] %>%
                    (function(x) str_sub(x, 2, nchar(x)))
            }

            extract_subs <- function(label) {
                str_split(label, "_")[[1]][-1] %>%
                    parse_subs()
            }

            parse_subs <- function(subs) {
                if (parse_action == "underscore") {
                    if (format == "plotmath") {
                        subs[subs == ""] <- 'paste("_")'
                        subs
                    } else {
                        subs[subs == ""] <- '_'
                        subs
                    }
                } else if (parse_action == "empty") {
                    subs[subs == ""] <- ""
                    subs
                } else if (parse_action == "remove") {
                    subs <- subs[subs != ""]
                    subs
                } else {
                    subs
                }
            }

            glue <- function(x) {
                if (format == "plotmath") {
                    paste0(
                        'paste(',
                        paste0(x, collapse = sep),
                        ')'
                    )
                } else {
                    paste0(
                        paste0(x, collapse = sep)
                    )
                }
            }

            superscript <- function(x,y) {
                paste0(x, "^", y)
            }

            subscript <- function(x,y) {
                if (format == "plotmath") {
                    paste0(x, "[", y, "]")
                } else {
                    paste0(x, "_{", y, "}")
                }

            }

            get_label_inner <- function(label) {
                if (format == "syntax") {
                    label
                } else if (is.na(label)) {
                    ""
                } else if (is_greek(label)) {
                    greek <- extract_greek_wo_number(label)
                    greek_number <- extract_greek_number(label)

                    if (is.null(greek_number)) {
                        greek
                    } else {
                        if (greek %in% c("pi", "Pi")) {
                            subscript(greek, greek_number-1)
                        } else {
                            subscript(greek, greek_number)
                        }
                    }
                } else if (starts_with_greek(label)) {
                    greek <- extract_greek(label)
                    subs <- extract_subs(label)

                    if (greek %in% c("sigma")) {
                        superscript(subscript(greek, glue(subs)), 2)
                    } else {
                        subscript(greek, glue(subs))
                    }
                } else if (str_detect(label, '^\\.pi[0-9]+')) {
                    index <- str_extract(label, '[0-9]+$') %>% as.integer()
                    subscript("pi", index-1)
                } else if (str_detect(label, '^\\.eta[0-9]+')) {
                    index <- str_extract(label, '[0-9]+$') %>%  as.integer()
                    subscript("eta", index)
                } else {
                    label
                }
            }

            sapply(labels, get_label_inner)
        },

        #' @description Returns a tibble that can be used to plot a DAG.
        #' @param labels Character. Label type on the edged.
        #' @param digits Integer. How many digits should be printed?
        #' @param ... Parameters passed to get_estimates().
        #' @import tidyverse
        get_graph_table = function(
        labels = c("both", "labels", "estimates"),
        digits = 3,
        ...
        ) {

            labels <- labels[1]

            group_labels <- private$group_labels
            n_groups <- length(groups)

            # some variables to calculate the positions from

            etas_mmodel <- private$etas_mmodel
            n_etas <- length(etas_mmodel)
            n_ind_etas <- first(sapply(etas_mmodel, length))
            n_ind_etas_total <- sum(sapply(etas_mmodel, length))

            covariates_mmodel <- private$covariates_mmodel
            n_covariates <- length(covariates_mmodel)
            if (n_covariates == 0L) {
                n_ind_covs <- 0
                n_ind_covs_total <- 0
            } else {
                n_ind_covs <- sapply(covariates_mmodel, length)
                n_ind_covs_total <- sum(n_ind_covs)
            }

            start_cov <- 1 + max(0, (n_ind_etas_total - n_ind_covs_total) / 2)
            start_eta <- 1 + max(0, (n_ind_covs_total - n_ind_etas_total) / 2)


            # get y, eta and pi positions

            etas_pi <- lapply(1:n_etas, function(eta_index) {
                eta <- names(etas_mmodel)[eta_index]
                indicators <- etas_mmodel[[eta_index]]

                if (n_ind_etas > 1L) {
                    result <- tibble(
                        lhs = paste0(c(".eta", ".pi"), eta_index),
                        x = c(2, 3),
                        y = start_eta + (eta_index - 1)*n_ind_etas + (n_ind_etas-1) / 2,
                        variable_type = "latent",
                        is_left = c(TRUE, FALSE)
                    )
                } else {
                    result <- tibble(
                        lhs = paste0(".pi", eta_index),
                        x = 3,
                        y = start_eta + (eta_index - 1)*n_ind_etas + (n_ind_etas-1) / 2,
                        variable_type = "latent",
                        is_left = c(FALSE)
                    )
                }


                for (ind_index in 1:n_ind_etas) {
                    indicator <- indicators[ind_index]
                    result <- bind_rows(
                        result,
                        tibble(
                            lhs = indicator,
                            x = if (n_ind_etas == 1L) 2 else 1,
                            y = start_eta + (eta_index - 1)*n_ind_etas + ind_index - 1,
                            variable_type = "manifest",
                            is_left = TRUE
                        )
                    )
                }
                result
            }) %>% bind_rows()


            # get covariates and indicator positions

            if (n_covariates > 0L) {
                covariates <- lapply(1:n_covariates, function(cov_index) {
                    covariate <- names(covariates_mmodel)[cov_index]
                    indicators <- covariates_mmodel[[cov_index]]
                    n_ind <- length(indicators)
                    start_inner <- sum(n_ind_covs[(1:n_covariates) < cov_index]) + start_cov

                    if (n_ind > 1L) {
                        result <- tibble(
                            lhs = covariate,
                            x = 4,
                            y = start_inner + (n_ind-1) / 2,
                            variable_type = "latent"
                        )
                    } else {
                        result <- tibble()
                    }

                    for (ind_index in 1:n_ind) {
                        indicator <- indicators[ind_index]
                        result <- bind_rows(
                            result,
                            tibble(
                                lhs = indicator,
                                x = if (n_ind == 1L) 4 else 5,
                                y = start_inner + ind_index - 1,
                                variable_type = "manifest"
                            )
                        )
                    }
                    result
                }) %>% bind_rows() %>%
                    mutate(is_left = FALSE)
            } else {
                covariates <- tibble()
            }

            coords <- bind_rows(
                covariates,
                etas_pi
            ) %>%
                mutate(from_id = 1:n())



            # join coordinates with parameter table

            pt <- self$get_estimates() %>%
                group_by(type) %>%
                group_modify(function(tbl, desc) {
                    if (desc$type != "regression") {
                        tbl
                    } else {
                        tbl %>%
                            mutate(lhs_tmp = lhs,
                                   lhs = rhs,
                                   rhs = lhs_tmp) %>%
                            select(-lhs_tmp)
                    }
                }) %>%
                ungroup()

            pt <- left_join(
                pt,
                coords,
                by = c("lhs")
            ) %>%
                mutate(to_id = sapply(rhs, function(x) first(from_id[lhs == x])))


            # add residuals

            n_resid_left <- pt %>%
                filter(
                    !private$is_latent(lhs),
                    private$is_indicator(lhs),
                    private$belongs_to(lhs, "eta"),
                    type == "resid_var",
                    group_index == 1
                ) %>% nrow()
            left_id <- max(pt$from_id, na.rm = T) + 1
            right_id <- left_id + n_resid_left

            pt <- pt %>%
                group_by(
                    is_resid_var = type == "resid_var" & private$is_indicator(lhs),
                    group,
                    left = private$belongs_to(lhs, "eta")
                ) %>%
                group_modify(function(tbl, desc) {
                    if (!desc$is_resid_var) {
                        tbl
                    } else if (desc$left) {
                        left_x <- 0
                        bind_rows(
                            tbl %>%
                                mutate(
                                    lhs = "epsilon",
                                    from_id = seq(left_id,left_id + nrow(tbl) - 1),
                                    to_id = NA,
                                    x = left_x,
                                    is_node = TRUE,
                                    variable_type = "latent"
                                ),
                            tbl %>%
                                mutate(
                                    rhs = lhs,
                                    lhs = "epsilon",
                                    to_id = from_id,
                                    label = NA,
                                    est = NA,
                                    se = NA,
                                    from_id = seq(left_id,left_id + nrow(tbl) - 1)
                                ),
                            tbl %>%
                                mutate(
                                    rhs = lhs,
                                    lhs = "epsilon",
                                    to_id = seq(left_id,left_id + nrow(tbl) - 1),
                                    from_id = seq(left_id,left_id + nrow(tbl) - 1)
                                )
                        )
                    } else if (!desc$left) {
                        right_x <- 6
                        bind_rows(
                            tbl %>%
                                mutate(
                                    lhs = "epsilon",
                                    from_id = seq(right_id,right_id + nrow(tbl) - 1),
                                    to_id = NA,
                                    x = right_x,
                                    is_node = TRUE,
                                    variable_type = "latent"
                                ),
                            tbl %>%
                                mutate(
                                    rhs = lhs,
                                    lhs = "epsilon",
                                    to_id = from_id,
                                    label = NA,
                                    est = NA,
                                    se = NA,
                                    from_id = seq(right_id,right_id + nrow(tbl) - 1),
                                ),
                            tbl %>%
                                mutate(
                                    rhs = lhs,
                                    lhs = "epsilon",
                                    to_id = seq(right_id,right_id + nrow(tbl) - 1),
                                    from_id = seq(right_id,right_id + nrow(tbl) - 1),
                                )
                        )
                    }
                }) %>%
                ungroup() %>%
                select(-left, -is_resid_var)


            # add intercept triangle

            max_y <- max(pt$y, na.rm = T)
            left_id <- max(pt$from_id, na.rm = T) + 1
            if (n_ind_etas == 1L) {
                right_id <- left_id
            } else {
                right_id <- left_id + 1
            }

            pt <- pt %>%
                group_by(op, group, left = lhs %in% unlist(etas_mmodel)) %>%
                group_modify(function(tbl, desc) {
                    if (desc$op != ~ 1) {
                        tbl
                    } else if (desc$left) {
                        if (n_ind_etas == 1L) {
                            tbl %>% mutate(is_node = TRUE, type = "node")
                        } else {
                            if (n_ind_etas == 1L) {
                                left_x <- 1
                            } else {
                                left_x <- -1
                            }
                            bind_rows(
                                tbl %>% mutate(is_node = TRUE, type = "node"),
                                tibble(
                                    lhs = "1",
                                    from_id = left_id,
                                    y = start_eta + (n_ind_etas_total - 1) / 2,
                                    x = left_x,
                                    type = "intercept",
                                    is_node = TRUE,
                                    variable_type = "intercept",
                                    is_left = TRUE
                                ),
                                tbl %>%
                                    mutate(
                                        rhs = lhs,
                                        lhs = "1",
                                        to_id = from_id,
                                        from_id = left_id,
                                        variable_type = "intercept",
                                        type = "intercept"
                                    )
                            )
                        }
                    } else if (!desc$left) {
                        if (sum(n_ind_covs) == 0L) {
                            right_x <- 4
                        } else if (max(n_ind_covs) == 1L) {
                            right_x <- 5
                        } else {
                            right_x <- 7
                        }
                        bind_rows(
                            tbl %>% mutate(is_node = TRUE, type = "node"),
                            tibble(
                                lhs = "1",
                                from_id = right_id,
                                y = start_cov + (n_ind_covs_total - 1) / 2,
                                x = right_x,
                                type = "intercept",
                                is_node = TRUE,
                                variable_type = "intercept",
                                is_left = FALSE
                            ),
                            tbl %>%
                                mutate(
                                    rhs = lhs,
                                    lhs = "1",
                                    to_id = from_id,
                                    from_id = right_id,
                                    variable_type = "intercept",
                                    type = "intercept"
                                )
                        )
                    }
                }) %>%
                ungroup() %>%
                select(-left)


            ## some labeling stuff

            pt <- pt %>%
                filter(!(rhs %in% names(private$etas_mmodel)) | type != "intercept") %>%
                mutate(
                    name = private$get_labels(lhs),
                    label = private$get_labels(label)
                ) %>%
                mutate(
                    is_node = ifelse(is.na(is_node), FALSE, TRUE),
                    est = round(est, !!digits),
                    est = case_when(
                        is.na(est) ~ "",
                        !free ~ paste0("ring(", est, ")"),
                        TRUE ~ as.character(est)
                    )
                ) %>%
                mutate(
                    label = case_when(
                        !!labels == "estimates" ~ as.character(est),
                        !!labels == "labels" ~ label,
                        !!labels == "both" & label == "" & est == "" ~ "",
                        !!labels == "both" & label != "" & est == "" ~ label,
                        !!labels == "both" & label == "" & est != "" ~ est,
                        !!labels == "both" & label != "" & est != "" ~ paste0(label, " == ", est),
                        TRUE ~ ""
                    )
                ) %>%
                mutate(y = -y + max_y) %>%
                filter(
                    free | !(type %in% c("variance", "resid_cov", "resid_var", "covariance"))
                )

            pt

        }

    ),
    active = list(

        #' @field get_par_table Read only. Retrieves the parameter table as tibble.
        get_par_table = function() {
            private$par_table$get_par_table
        },

        #' @field get_group_labels Read only. Retrieves the group_labels.
        get_group_labels = function() {
            private$group_labels
        },

        #' @field expose Read only. Returns the private structure of the object. For debugging purposes only.
        expose = function() {
            private
        },

        #' @field get_hypotheses Read only. Returns the list of hypotheses. For debugging purposes only.
        get_hypotheses = function() {
            private$hypotheses
        },

        #' @field get_par_labels Read only. Returns an array of parameter labels used in the lavaan syntax.
        get_par_labels = function() {
            private$par_table$get_par_labels(
                pis = private$pis,
                groups = private$group_labels,
                covariates = private$covariates
            )
        },

        #' @field get_sem_object Read only. Returns the estimated lavaan SEM object.
        get_sem_object = function() {
            private$sem_obj
        }

    ),
    public = list(

        #' @description Lgc class constructor.
        #' @export
        initialize = function() {
            invisible(self)
        },

        #' @description Specifies an Lgc object.
        #' @param data tibble. Will be converted to tibble if not already a tibble.
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
        #' @param c_matrix_within Numeric q x q matrix. Contrast matrix for the
        #' within-subjects design. Must be a square matrix.
        #' E.g.:
        #' \code{
        #' c_matrix_within = matrix(c(
        #'     1/3, -sqrt(1/2),  sqrt(1/6),
        #'     1/3,          0, -sqrt(2/3),
        #'     1/3,  sqrt(1/2),  sqrt(1/6)
        #' ), nrow = 3)}
        #' @param resid_cov List of character vectors. Each character vector
        #' contains manifest variables from the data set among which residual
        #' covariances should be implemented. Residual covariances can vary.
        #' @param equal_resid_cov List of character vectors. Each character vector
        #' contains manifest variables from the data set among which residual
        #' covariances should be implemented. Residual covariances are
        #' constrained to be equal.
        #' @param invariance_within Logical. Default is TRUE. Indicates whether
        #' strong measurement invariance across latent dependent variables eta
        #' should be imposed.
        #' @param compound_symmetry Logical. Indicates whether compound symmetry
        #' should be imposed.
        #' @param sphericity List of character vectors. Each character vector
        #' contains indices of pi variables among which sphericity should be
        #' imposed.
        #' @param group Character. Contains the grouping variable from the data
        #' set.
        #' @param group_labels Character vector of length p. Contains unique group
        #' names of the group variable from the data set. Optional, default is
        #' NULL. If not supplied, labels will be pulled from the data set using
        #' the provided group variable.
        #' E.g.: \code{groups = c("group1", "group2", "group3")}
        #' @param c_matrix_between Numeric p x p matrix. Contrast matrix for the
        #' between-subjects design. Must be a square matrix.
        #' E.g.:
        #' \code{
        #' c_matrix_between = matrix(c(
        #'     1/3, -sqrt(1/2),  sqrt(1/6),
        #'     1/3,          0, -sqrt(2/3),
        #'     1/3,  sqrt(1/2),  sqrt(1/6)
        #' ), nrow = 3)}
        #' @param invariance_between Logical. Default is \code{TRUE}.
        #' Indicates whether measurement invariance across groups should be
        #' imposed.
        #' @param covariates Named list of character vectors. Same format as the
        #' argument etas.
        #' @param fixed_covariates Logical. Indicates whether manifest
        #' covariates should be treated as fixed.
        #' @param lv_scaling Character. Possible values:
        #' \code{c("effect", "referent")}. Default is "effect". Scheme to
        #' identify the latent variable scale. For \code{"effect"}: effect coding
        #' scheme freely estimates all loadings and intercepts of the indicators
        #' and constraints the average of the loadings to 1 and the average of
        #' the intercepts to 0. For \code{"referent"}: referent indicator scheme
        #' sets the first loading to 1 and the first intercept to 0.
        #' @param group_weights Either numeric vector of length p (same length
        #' as \code{group_labels}) or one of the characters
        #' \code{c("fixed", "stochastic")}. Default is \code{"fixed"}.
        #' @param variance_homogeneity Logical. Default is \code{TRUE}.
        #' Indicates whether homogeneity of variance should be imposed across
        #' groups.
        #' @param hypotheses List of objects of class Hypothesis.
        #' @param append Character. lavaan syntax that is to be appended to the
        #' model string.
        #' @param dummy Logical. Indicates whether the model should be
        #' estimated.
        #' @param verbose Logical.
        #' @param ... Additional arguments passed down to lavaan.
        #' @export
        #' @import tidyverse
        #' @importFrom lavaan sem
        specify = function(
        data = NULL,
        etas,
        c_matrix_within,
        resid_cov = list(),
        equal_resid_cov = list(),
        invariance_within = "strong",
        compound_symmetry = FALSE,
        sphericity = list(),
        c_matrix_between = TRUE,
        invariance_between = "strong",
        covariates = NULL,
        fixed_covariates = TRUE,
        lv_scaling = "effect",
        group = NULL,
        group_labels = NULL,
        group_weights = "fixed",
        variance_homogeneity = FALSE,
        hypotheses = NULL,
        append = NULL,
        dummy = FALSE,
        verbose = FALSE,
        ...
        ) {

            private$options <- Options$new()
            private$par_table <- ParTable$new()

            # initialize: 1. some parameter checking ---------------------------

            private$validate(
                data = data,
                etas = etas,
                c_matrix_within = c_matrix_within,
                resid_cov = resid_cov,
                equal_resid_cov = equal_resid_cov,
                invariance_within = invariance_within,
                compound_symmetry = compound_symmetry,
                sphericity = sphericity,
                c_matrix_between = c_matrix_between,
                invariance_between = invariance_between,
                covariates = covariates,
                fixed_covariates = fixed_covariates,
                lv_scaling = lv_scaling,
                group = group,
                group_labels = group_labels,
                group_weights = group_weights,
                variance_homogeneity = variance_homogeneity,
                hypotheses = hypotheses,
                append = append,
                dummy = dummy
            )

            # initialize: 2. specify model -------------------------------------

            private$par_table$specify_measurement_model_eta(
                etas = private$etas_mmodel,
                invariance_within = private$invariance_within,
                invariance_between = private$invariance_between,
                groups = private$group_labels,
                lv_scaling = private$lv_scaling
            )

            private$par_table$specify_resid_cov(
                etas = private$etas_mmodel,
                groups = private$group_labels,
                resid_cov = private$resid_cov,
                equal_resid_cov = private$equal_resid_cov
            )

            private$par_table$specify_etas(
                etas = private$etas_mmodel,
                groups = private$group_labels,
                compound_symmetry = private$compound_symmetry,
                variance_homogeneity = private$variance_homogeneity
            )

            private$par_table$specify_pis(
                etas = private$etas_mmodel,
                pis = private$pis,
                c_matrix_within = private$c_matrix_within,
                groups = private$group_labels,
                sphericity = private$sphericity,
                compound_symmetry = private$compound_symmetry,
                variance_homogeneity = private$variance_homogeneity
            )

            private$par_table$specify_group_weights(
                groups = private$group_labels,
                group_weights = private$group_weights
            )

            private$par_table$specify_measurement_model_covariates(
                covariates = private$covariates_mmodel,
                pis = private$pis,
                invariance_between = private$invariance_between,
                group = private$group_labels,
                lv_scaling = private$lv_scaling
            )

            # initialize: 3. estimate model ------------------------------------

            if (verbose) {
                cat(self$get_lav_syntax())
            }

            if (!dummy) {
                self$estimate(...)
            }

            invisible(self)

        },

        specify_dummy = function(
        n_etas = 1,
        n_indicators_eta = 1,
        n_groups = 1,
        c_matrix_within = NULL,
        n_covariates = NULL,
        hypotheses = NULL,
        ...
        ) {

            if (is.null(c_matrix_within)) {
                if (n_etas == 1L) {
                    c_matrix_within <- matrix(1)
                } else {
                    c_matrix_within <- solve(cbind(1, contr.poly(n_etas)))
                }
            }

            # create eta measurement model

            etas <- lapply(1:n_etas, function(eta_index) paste0("Y", 1:n_indicators_eta, "_eta", eta_index))
            names(etas) <- paste0("eta", 1:n_etas)

            # create covariates measurement model

            if (!is.null(n_covariates) && sum(n_covariates) > 0L) {
                n_covariate_inds <- n_covariates
                n_covariates <- length(n_covariates)
                covariates <- lapply(1:n_covariates, function(covariate_index) {
                    n_indicators <- n_covariate_inds[covariate_index]
                    paste0("Y", 1:n_indicators, "_cov", covariate_index)
                })
                names(covariates) <- paste0("cov", 1:n_covariates)
            } else {
                covariates <- NULL
            }

            # create groups

            if (is.null(n_groups) || n_groups == 1L) {
                n_groups <- 1
            }
            groups <- paste0("g", 1:n_groups)

            # create data syntax

            if (is.null(hypotheses)) {
                M <- matrix(c(1, rep_len(0, n_etas-1)), ncol = 1)
                L <- matrix(c(1, rep_len(0, n_groups-1)), nrow = 1)
                P <- matrix(c(1, rep_len(0, max(0, n_covariates))), nrow = 1)

                hypotheses <- list(
                    Hypothesis$new(
                        description = "dummy",
                        M = M,
                        L = L,
                        P = P
                    )
                )
            }

            self$specify(
                c_matrix_within = c_matrix_within,
                etas = etas,
                covariates = covariates,
                group_labels = groups,
                group = ".group",
                hypotheses = hypotheses,
                dummy = TRUE
            )

            invisible(self)
        },

        #' @description Estimates the model.
        #' @param ... Additional arguments.
        estimate = function(...) {
            if (is.null(private$data)) {
                return(invisible(self))
            }

            private$sem_obj <- lavaan::sem(
                model = paste0(self$get_lav_syntax(), "\n", private$append),
                data = private$data,
                group = private$group,
                group.label = private$group_labels,
                ...
            )
        },

        #' @description Retrieves the lavaan syntax.
        #' @param ... Additional arguments.
        get_lav_syntax = function(...) {
            private$par_table$get_lav_syntax(...)
        },

        #' @description Returns the point estimates of the model.
        #' @param only_unique_labels Logical. Default is FALSE.
        #' @param what Character vector.
        #' @param ... Additional arguments.
        #' @importFrom lavaan parTable
        #' @importFrom stringr str_detect str_extract
        get_estimates = function(
        what = c(
            "variance", "covariance", "intercept", "measurement",
            "resid_var", "resid_cov", "regression"),
        only_unique_labels = FALSE,
        ...
        ) {

            if (first(what) == "all") {
                what <- c(
                    "variance", "covariance", "intercept", "measurement",
                    "resid_var", "resid_cov", "regression", "group_size"
                )
            }

            group_labels <- private$group_labels

            eta_indicators <- unlist(private$etas_mmodel)
            etas <- names(private$etas_mmodel)

            covariates <- names(private$covariates_mmodel)
            cov_indicators <- sapply(
                private$covariates_mmodel,
                function(indicators) {
                    if (length(indicators) > 1L) {
                        indicators
                    } else {
                        NULL
                    }
                }
            ) %>% unlist()

            # pt <- parTable(self$get_sem_object) %>%
            #     as_tibble() %>%
            #     mutate(pvalue = ifelse(free == 0L, NA, 2*pnorm(-abs(est/se)))) %>%
            #     filter(group > 0L | str_detect(lhs, "^relfreq") | str_detect(lhs, "^\\.[il]_")) %>%
            #     select(lhs, op, rhs, group, free, label, est, se, pvalue)
            #
            # pt <- pt %>%
            #     mutate(
            #         group = case_when(
            #             op == ":=" & str_detect(lhs, "^relfreq") ~ as.integer(str_extract(lhs, "[0-9]+$")),
            #             TRUE ~ group
            #         ),
            #         group_index = group,
            #         group = sapply(group_index, function(x) if (x == 0L) NA_character_ else group_labels[x]),
            #         free = ifelse(free == 0L, FALSE, TRUE),
            #         type = case_when(
            #             op == "~1" ~ "intercept",
            #             op == "=~" ~ "measurement",
            #             op == "~" ~ "regression",
            #             op == "==" ~ "constraint",
            #             op == "~~" & private$is_latent(lhs) & rhs == lhs ~ "variance",
            #             op == "~~" & private$is_latent(lhs) & rhs != lhs ~ "covariance",
            #             op == "~~" & !private$is_latent(lhs) & rhs == lhs ~ "resid_var",
            #             op == "~~" & !private$is_latent(lhs) & rhs != lhs ~ "resid_cov",
            #             op == ":=" & str_detect(lhs, "^relfreq") ~ "group_size",
            #             op == ":=" & str_detect(lhs, "^.\\nu_cov_") ~ "intercept",
            #             TRUE ~ NA_character_
            #         )
            #     ) %>%
            #     select(lhs,op,rhs,group,free,label,est,se,group_index,type) %>%
            #     arrange(lhs, rhs, op, group)
            #
            # pt %>%
            #     arrange(lhs, rhs, op, group) %>%
            #     kableExtra::kable(format = "simple")

            pt2 <- private$par_table$get_par_table %>%
                group_by(lhs, rhs, op, group) %>%
                group_modify(function(tbl, desc) {
                    max_index <- max(1, which.max(nchar(tbl$value)))
                    tbl %>%
                        mutate(value = value[max_index]) %>%
                        filter(!na)
                }) %>%
                ungroup() %>%
                filter(!na) %>%
                arrange(op, lhs, rhs) %>%
                mutate(
                    group_index = sapply(group, function(x) if (is.na(x)) 0L else which(x == private$group_labels)),
                    est = suppressWarnings(as.numeric(value)),
                    se = NA_real_,
                    pvalue = NA_real_,
                    rhs = ifelse(is.na(rhs), "", rhs),
                    label = ifelse(private$is_label(value), value, ""),
                    free = 1L
                ) %>%
                select(lhs, rhs, free, group_index, label, est, se, pvalue, op)

            if (!is.null(private$sem_obj)) {
                pt_lav <- parTable(self$get_sem_object) %>%
                    mutate(pvalue = ifelse(free == 0L, NA, 2*pnorm(-abs(est/se)))) %>%
                    as_tibble() %>%
                    arrange(op, lhs, rhs) %>%
                    mutate(
                        group_index = group,
                    ) %>%
                    select(lhs, rhs, op, group_index, est, free, se, pvalue) %>%
                    filter(!str_detect(lhs, "^\\.p[0-9]+\\.$"))

                pt2 <- full_join(
                    pt2 %>% select(-est, -se, -pvalue, -free),
                    pt_lav,
                    by = c("lhs", "op", "rhs", "group_index")
                ) %>%
                    filter(group_index > 0L | str_detect(lhs, "^relfreq"))
            }

            pt2 <- pt2 %>%
                mutate(
                    group_index = case_when(
                        op == ":=" & str_detect(lhs, "^relfreq") ~ as.integer(str_extract(lhs, "[0-9]+$")),
                        TRUE ~ group_index
                    ),
                    group = sapply(group_index, function(x) if (x == 0L) NA_character_ else group_labels[x]),
                    free = ifelse(free == 0L, FALSE, TRUE),
                    type = case_when(
                        op == "~1" ~ "intercept",
                        op == "=~" ~ "measurement",
                        op == "~" ~ "regression",
                        op == "==" ~ "constraint",
                        op == "~~" & private$is_latent(lhs) & rhs == lhs ~ "variance",
                        op == "~~" & private$is_latent(lhs) & rhs != lhs ~ "covariance",
                        op == "~~" & !private$is_latent(lhs) & rhs == lhs ~ "resid_var",
                        op == "~~" & !private$is_latent(lhs) & rhs != lhs ~ "resid_cov",
                        op == ":=" & str_detect(lhs, "^relfreq") ~ "group_size",
                        op == ":=" & str_detect(lhs, "^.\\nu_cov_") ~ "intercept",
                        TRUE ~ NA_character_
                    )
                ) %>%
                select(lhs, op, rhs, group, free, label, est, se, group_index, type, pvalue) %>%
                arrange(lhs, rhs, op, group)

            # pt2 %>%
            #     select(lhs,op,rhs,group,free,label,est,se,group_index,type) %>%
            #     arrange(lhs, rhs, op, group) %>%
            #     kableExtra::kable(format = "simple")

            pt <- pt2

            pt <- pt %>%
                filter(!(type == "variance" & !free)) %>%
                mutate(
                    label = ifelse(
                        label == "" & lhs %in% covariates & type %in% c("intercept", "variance"),
                        paste0(".nu_cov_",
                               sapply(lhs, function(x)
                                   which(x == covariates)),
                               "__",
                               group_index),
                        label
                    )
                ) %>%
                filter(type %in% c("regression", "measurement") | type != "") %>%
                filter(type %in% what)

            if (only_unique_labels) {
                pt %>%
                    select(label, est, se, pvalue) %>%
                    group_by(label) %>%
                    summarize(est = first(est),
                              se = first(se))
            } else {
                pt
            }
        },

        #' @description Tests the provided hypothesis on the lgc object.
        #' @export
        #' @param hypothesis Object of class Hypothesis.
        #' @importFrom lavaan lavTestWald
        test = function(hypothesis) {

            wald_string <- hypothesis$get_wald_string(self)

            test <- lavaan::lavTestWald(private$sem_obj, wald_string)
            test$constraints <- wald_string

            test
        },

        #' @description Tests multiple hypotheses on the lgc object and returns
        #' the results as a table.
        #' @export
        #' @param hypothesis List of objects of class Hypothesis.
        #' @importFrom lavaan lavTestWald
        tests_table = function(hypotheses) {
            result <- matrix(NA_real_, ncol = 3, nrow = length(hypotheses))

            for (hypothesis_index in 1:length(hypotheses)) {
                hypothesis <- hypotheses[[hypothesis_index]]

                wald <- self$test(hypothesis)

                result[hypothesis_index,] <- c(
                    round(wald$stat,3),
                    wald$df,
                    round(wald$p.value,3)
                )
            }

            rownames(result) <- lapply(hypotheses, function(x) x$get_description)
            colnames(result) <- c("Chisq", "df", "Pr(>Chisq)")
            result <- as.data.frame(result)
            class(result) <- c("anova", "data.frame")

            result <- structure(
                result,
                heading = "Main and Interaction Effects:",
                class = c("anova", "data.frame")
            )

            result
        },

        #' @description Prints the lavaan object.
        #' @export
        #' @importFrom kableExtra kable
        print = function(...) {
            if (is.null(private$sem_obj)) {
                return(invisible(self))
            }

            self$get_estimates() %>%
                filter(free) %>%
                select(parameter = label, est, est, group, se, pvalue) %>%
                mutate(parameter = private$get_labels(parameter, format = "text")) %>%
                group_by(group) %>%
                group_walk(function(tbl, desc) {
                    # cat("Group: ")
                    # cat(desc$group)
                    # cat("\n")
                    kableExtra::kable(
                        tbl,
                        format = "simple",
                        caption = if (length(private$group_labels) == 1L) NULL else desc$group
                    ) %>%
                        print()

                    invisible()
                })

            invisible(self)
        },

        #' @description Prints a summary for the Lgc object.
        #' @export
        #' @param detailed Logical. Indicates whether a detailed summary
        #' should be printed.
        #' @param fit_measures Logical. Indicates whether fit measures should be
        #' printed.
        #' @importFrom lavaan fitmeasures
        summary = function(detailed = F, fit_measures = T) {

            if (lavaan::fitmeasures(private$sem_obj)[c("df")] > 0L && fit_measures) {
                cat("Fit measures:\n\n")

                cat(paste0(
                    "Chisq(",
                    lavaan::fitmeasures(private$sem_obj)[c("df")],
                    ") = ",
                    round(lavaan::fitmeasures(private$sem_obj)[c("chisq")],3),
                    ", p = ",
                    round(lavaan::fitmeasures(private$sem_obj)[c("pvalue")],3),
                    "\n"
                ))

                cat(paste0(
                    "CFI = ",
                    round(lavaan::fitmeasures(private$sem_obj)[c("cfi")], 3),
                    "\n"
                ))

                cat(paste0(
                    "TLI = ",
                    round(lavaan::fitmeasures(private$sem_obj)[c("tli")], 3),
                    "\n"
                ))

                cat(paste0(
                    "RMSEA = ",
                    round(lavaan::fitmeasures(private$sem_obj)[c("rmsea")], 3),
                    " [",
                    round(lavaan::fitmeasures(private$sem_obj)[c("rmsea.ci.lower")], 3),
                    ", ",
                    round(lavaan::fitmeasures(private$sem_obj)[c("rmsea.ci.upper")], 3),
                    "]\n"
                ))

                cat("\n##############################\n\n")
            }

            if (detailed) {
                for (hypothesis_index in 1:length(private$hypotheses)) {

                    hypothesis <- private$hypotheses[[hypothesis_index]]

                    if (hypothesis_index > 1L) {
                        cat("\n##############################\n\n")
                    }

                    wald <- self$test(hypothesis)
                    cat(paste0(
                        "Hypothesis: ",
                        hypothesis$get_description,
                        "\n\nM Matrix:\n"
                    ))
                    print(hypothesis$get_M)
                    cat("\nL Matrix:\n")
                    print(hypothesis$get_L)
                    cat("\nP Matrix:\n")
                    print(hypothesis$get_P)
                    cat(paste0(
                        "\nConstraints:\n",
                        wald$constraints,
                        "\n\nchisq(",
                        wald$df,
                        ") = ",
                        round(wald$stat, 3),
                        ", p = ",
                        round(wald$p.value, 3),
                        "\n"
                    ))

                }
            } else if (length(private$hypotheses) > 0L) {

                print(self$tests_table(private$hypotheses))

            }

            invisible(self)
        },

        #' @description Compares the lavaan objects contained in Lgc objects.
        #' @export
        #' @param ... Additional arguments passed down to
        #' anova(lavaan_object, ...).
        #' @importFrom lavaan lavTestLRT
        compare = function(...) {

            # Mostly copied from the lavaan package.
            # Thanks, Yves!!

            object <- self$get_sem_object

            mcall <- match.call(expand.dots = TRUE)
            dots <- list(...)

            SB.classic <- TRUE; SB.H0 <- FALSE

            arg.names <- names(dots)
            arg.idx <- which(nchar(arg.names) > 0L)
            if(length(arg.idx) > 0L) {
                if(!is.null(dots$SB.classic))
                    SB.classic <- dots$SB.classic
                if(!is.null(dots$SB.H0))
                    SB.H0 <- dots$SB.H0
                dots <- dots[-arg.idx]
            }

            modp <- sapply(dots, inherits, "Lgc")
            mods <- c(list(object), dots[modp])
            NAMES <- c(sapply(as.list(mcall)[c(FALSE, TRUE, modp)], deparse))
            dots <- lapply(dots, function(x) if (inherits(x, "Lgc")) x$get_sem_object else x)

            ans <- do.call("lavaan::lavTestLRT", c(list(object = object,
                                                        SB.classic = SB.classic, SB.H0 = SB.H0,
                                                        model.names = NAMES), dots))
            ans
        },

        #' @description Plots a directed acyclic graph for the Lgc object.
        #' @export
        #' @param ... Arguments passed to get_tidy_dagitty().
        #' @import tidyverse
        #' @import ggraph
        #' @import tidygraph
        plot = function(
        label = "both",
        what = c("regression", "measurement"),
        variance.direction = 90,
        variance.span = 75,
        variance.strength = 1.5,
        resid_var_left.direction = 180,
        resid_var_left.span = -75,
        resid_var_left.strength = 1.5,
        resid_var_right.direction = 0,
        resid_var_right.span = 75,
        resid_var_right.strength = 1.5,
        covariance.strength = 0.1,
        transparent_nodes = FALSE,
        only_first_group = FALSE,
        pi.x = NULL,
        eta.x = NULL,
        eta_indicator.x = NULL,
        covariate.x = NULL,
        covariate_indicator.x = NULL,
        left_intercept.x = NULL,
        right_intercept.x = NULL,
        left_residual.x = NULL,
        right_residual.x = NULL,
        node_distance_scale.x = 1,
        node_distance_scale.y = 1,
        vjust = -0.5,
        ...
        ) {

            validata_what <- function(what_internal) {
                plot_components <- c(
                    "variance", "covariance", "intercept", "measurement",
                    "resid_var", "resid_cov", "regression", "group_size"
                )

                if (is.null(what_internal) || !is.character(what_internal)) {
                    what_internal <- plot_components
                }

                if (any(what_internal == "all")) {
                    what_internal <- plot_components
                }

                what_internal <- what_internal[sapply(what_internal, function(x) x %in% plot_components)]

                if (length(what_internal) == 0L) {
                    what_internal <- plot_components
                }

                what_internal
            }

            what <- validata_what(what)

            pt <- private$get_graph_table() %>%
                filter(group == first(private$group_labels) | !only_first_group) %>%
                mutate(x = case_when(
                    is_node & is_left & type == "intercept" ~
                        # ifelse(!is.null(left_intercept.x), left_intercept.x, x),
                        sapply(x, function(x) if(!is.null(left_intercept.x)) left_intercept.x else x),
                    is_node & !is_left & type == "intercept" ~
                        sapply(x, function(x) if(!is.null(right_intercept.x)) right_intercept.x else x),
                    is_node & is_left & type == "resid_var" ~
                        # ifelse(!is.null(left_residual.x), left_residual.x, x),
                        sapply(x, function(x) if(!is.null(left_residual.x)) left_residual.x else x),
                    is_node & !is_left & type == "resid_var" ~
                        # ifelse(!is.null(right_residual.x), right_residual.x, x),
                        sapply(x, function(x) if(!is.null(right_residual.x)) right_residual.x else x),
                    is_node & private$belongs_to(lhs, "pi") ~
                        # ifelse(!is.null(pi.x), pi.x, x),
                        sapply(x, function(x) if(!is.null(pi.x)) pi.x else x),
                    is_node & private$belongs_to(lhs, "eta") & !private$is_indicator(lhs) ~
                        # ifelse(!is.null(eta.x), eta.x, x),
                        sapply(x, function(x) if(!is.null(eta.x)) eta.x else x),
                    is_node & private$belongs_to(lhs, "eta") & private$is_indicator(lhs) ~
                        # ifelse(!is.null(eta_indicator.x), eta_indicator.x, x),
                        sapply(x, function(x) if(!is.null(eta_indicator.x)) eta_indicator.x else x),
                    is_node & private$belongs_to(lhs, "covariate") & !private$is_indicator(lhs) ~
                        # ifelse(!is.null(covariate.x), covariate.x, x),
                        sapply(x, function(x) if(!is.null(covariate.x)) covariate.x else x),
                    is_node & private$belongs_to(lhs, "covariate") & private$is_indicator(lhs) ~
                        # ifelse(!is.null(covariate_indicator.x), covariate_indicator.x, x),
                        sapply(x, function(x) if(!is.null(covariate_indicator.x)) covariate_indicator.x else x),
                    TRUE ~ x
                )) %>%
                mutate(
                    y = (y-min(y, na.rm = T)) * node_distance_scale.y + min(y, na.rm = T),
                    x = (x-min(x, na.rm = T)) * node_distance_scale.x + min(x, na.rm = T)
                )


            coord <- pt %>%
                ungroup() %>%
                filter(is_node) %>%
                mutate(myfilter = type %in% !!what | type == "node") %>%
                # filter(variable_type != "intercept" | "intercept" %in% what) %>%
                select(id = from_id, x, y, group, type, lhs, rhs, is_node, myfilter) %>%
                arrange(group, id)

            nodes <- pt %>%
                filter(is_node) %>%
                mutate(myfilter = type %in% !!what | type == "node") %>%
                # filter(variable_type != "intercept" | "intercept" %in% what) %>%
                select(from_id, name, group, type, variable_type, myfilter, is_node) %>%
                arrange(group, from_id)

            edges <- pt %>%
                ungroup() %>%
                filter(is.na(is_node) | !is_node) %>%
                select(from = from_id, to = to_id, label = label, group, type, is_left)

            ylimits <- coord %>%
                ungroup() %>%
                filter(myfilter) %>%
                summarize(
                    min = min(y, na.rm = T) - 0.5 * node_distance_scale.y,
                    max = max(y, na.rm = T) + 0.5 * node_distance_scale.y
                ) %>% as.list() %>% unlist()

            xlimits <- coord %>%
                ungroup() %>%
                filter(myfilter) %>%
                summarize(
                    min = min(x, na.rm = T) - 0.5 * node_distance_scale.x,
                    max = max(x, na.rm = T) + 0.5 * node_distance_scale.x
                ) %>% as.list() %>% unlist()

            gr <- tbl_graph(nodes, edges = edges)

            myplot <- ggraph(
                gr,
                layout = "manual",
                circular = FALSE,
                x = coord$x,
                y = coord$y
            ) +
                theme_classic() +
                theme(
                    legend.position = "none",
                    axis.line = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.spacing = unit(0, "lines"),
                    plot.margin = unit(c(0,0,0,0), "lines"),
                    strip.switch.pad.grid = unit(0, "lines"),
                    strip.switch.pad.wrap = unit(0, "lines")
                ) +
                ylim(ylimits) +
                xlim(xlimits)


            add_plot_component <- function(myplot, what_internal) {
                if ("variance" == what_internal) {
                    myplot +
                        geom_edge_loop(
                            mapping = aes(
                                strength = variance.strength,
                                label = label,
                                direction = variance.direction,
                                span = variance.span,
                                filter = type == "variance",
                                vjust = vjust
                            ),
                            label_size = 3,
                            arrow = arrow(
                                angle = 30,
                                length = unit(0.3, "cm"),
                                ends = "last",
                                type = "closed"
                            ),
                            end_cap = circle(0.7, 'cm'),
                            start_cap = circle(0.7, 'cm'),
                            label_parse = T,
                            color = "black",
                            label_colour = "black"
                        )
                } else if ("covariance" == what_internal) {
                    if (private$has_within()) {
                        myplot +
                            geom_edge_arc(
                                mapping = aes(
                                    label = label,
                                    label_pos = 0.3,
                                    filter = type == "covariance",
                                    vjust = vjust
                                ),
                                label_size = 3,
                                strength = covariance.strength,
                                arrow = arrow(
                                    angle = 30,
                                    length = unit(0.3, "cm"),
                                    ends = "both",
                                    type = "closed"
                                ),
                                end_cap = circle(0.8, 'cm'),
                                start_cap = circle(0.8, 'cm'),
                                label_parse = T,
                                color = "black",
                                label_colour = "black"
                            )
                    } else {
                        myplot
                    }
                } else if ("resid_var" == what_internal) {
                    if (private$has_latent_dv()) {
                        myplot <- myplot +
                            geom_edge_arc(
                                mapping = aes(
                                    label = label,
                                    label_pos = 0.3,
                                    filter = type == "resid_var",
                                    vjust = vjust
                                ),
                                label_size = 3,
                                strength = 0,
                                arrow = arrow(
                                    angle = 30,
                                    length = unit(0.3, "cm"),
                                    ends = "last",
                                    type = "closed"
                                ),
                                end_cap = circle(0.8, 'cm'),
                                start_cap = circle(0.8, 'cm'),
                                label_parse = T,
                                color = "black",
                                label_colour = "black"
                            ) +
                            geom_edge_loop(
                                mapping = aes(
                                    label = label,
                                    strength = resid_var_left.strength,
                                    direction = resid_var_left.direction,
                                    span = resid_var_left.span,
                                    filter = (type == "resid_var" & is_left),
                                    vjust = vjust
                                ),
                                label_size = 3,
                                arrow = arrow(
                                    angle = 30,
                                    length = unit(0.3, "cm"),
                                    ends = "last",
                                    type = "closed"
                                ),
                                end_cap = circle(0.7, 'cm'),
                                start_cap = circle(0.7, 'cm'),
                                label_parse = T,
                                color = "black",
                                label_colour = "black"
                            )
                    }
                    if (private$has_latent_covariate()) {
                        myplot <- myplot +
                            geom_edge_arc(
                                mapping = aes(
                                    label = label,
                                    label_pos = 0.3,
                                    filter = (type == "resid_var" & !is_left),
                                    vjust = vjust
                                ),
                                label_size = 3,
                                strength = 0,
                                arrow = arrow(
                                    angle = 30,
                                    length = unit(0.3, "cm"),
                                    ends = "last",
                                    type = "closed"
                                ),
                                end_cap = circle(0.8, 'cm'),
                                start_cap = circle(0.8, 'cm'),
                                label_parse = T,
                                color = "black",
                                label_colour = "black"
                            ) +
                            geom_edge_loop(
                                mapping = aes(
                                    label = label,
                                    strength = resid_var_right.strength,
                                    direction = resid_var_right.direction,
                                    span = resid_var_right.span,
                                    filter = (type == "resid_var" & !is_left),
                                    vjust = vjust
                                ),
                                label_size = 3,
                                arrow = arrow(
                                    angle = 30,
                                    length = unit(0.3, "cm"),
                                    ends = "last",
                                    type = "closed"
                                ),
                                end_cap = circle(0.7, 'cm'),
                                start_cap = circle(0.7, 'cm'),
                                label_parse = T,
                                color = "black",
                                label_colour = "black"
                            )
                    }
                    myplot
                } else if ("resid_cov" == what_internal) {
                    if (private$has_latent_dv() && private$has_resid_cov()) {
                        myplot +
                            geom_edge_arc(
                                mapping = aes(
                                    label = label,
                                    label_pos = 0.3,
                                    filter = type == "resid_cov",
                                    vjust = vjust
                                ),
                                label_size = 3,
                                strength = -0.3,
                                arrow = arrow(
                                    angle = 30,
                                    length = unit(0.3, "cm"),
                                    ends = "last",
                                    type = "closed"
                                ),
                                end_cap = circle(0.8, 'cm'),
                                start_cap = circle(0.8, 'cm'),
                                label_parse = T,
                                color = "black",
                                label_colour = "black"
                            )
                    } else {
                        myplot
                    }
                } else if ("regression" == what_internal) {
                    if (private$has_covariate()) {
                        myplot +
                            geom_edge_arc(
                                mapping = aes(
                                    label = label,
                                    label_pos = 0.3,
                                    filter = type %in% "regression",
                                    vjust = vjust
                                ),
                                strength = 0,
                                label_size = 3,
                                arrow = arrow(
                                    angle = 30,
                                    length = unit(0.3, "cm"),
                                    ends = "last",
                                    type = "closed"
                                ),
                                end_cap = circle(0.8, 'cm'),
                                start_cap = circle(0.8, 'cm'),
                                label_parse = T
                            )
                    } else {
                        myplot
                    }
                } else if ("measurement" == what_internal) {
                    myplot +
                        geom_edge_arc(
                            mapping = aes(
                                label = label,
                                label_pos = 0.3,
                                filter = type %in% "measurement",
                                vjust = vjust
                            ),
                            strength = 0,
                            label_size = 3,
                            arrow = arrow(
                                angle = 30,
                                length = unit(0.3, "cm"),
                                ends = "last",
                                type = "closed"
                            ),
                            end_cap = circle(0.8, 'cm'),
                            start_cap = circle(0.8, 'cm'),
                            label_parse = T
                        )
                } else if ("intercept" == what_internal) {
                    myplot +
                        geom_edge_arc(
                            mapping = aes(
                                label = label,
                                label_pos = 0.3,
                                filter = type == "intercept",
                                vjust = vjust
                            ),
                            strength = 0,
                            label_size = 3,
                            arrow = arrow(
                                angle = 30,
                                length = unit(0.3, "cm"),
                                ends = "last",
                                type = "closed"
                            ),
                            end_cap = circle(0.8, 'cm'),
                            start_cap = circle(0.8, 'cm'),
                            label_parse = T,
                            color = "black",
                            label_colour = "black"
                        )
                } else {
                    myplot
                }
            }

            for (what_internal in what) {
                myplot <- add_plot_component(myplot, what_internal)
            }

            if (transparent_nodes) {
                myplot <- myplot +
                    geom_node_point(aes(shape = variable_type, filter = myfilter), size = 15) +
                    scale_shape_manual(values=c(`manifest` = 0, `latent` = 1, `intercept` = 2)) +
                    geom_node_text(aes(label = name, filter = myfilter), parse = T)
            } else {
                myplot <- myplot +
                    geom_node_point(aes(shape = variable_type, filter = myfilter, fill = "white"), size = 15, fill = "white") +
                    scale_shape_manual(values=c(`manifest` = 22, `latent` = 21, `intercept` = 24)) +
                    geom_node_text(aes(label = name, filter = myfilter), parse = T)
            }

            if (length(private$group_labels) > 1L && !only_first_group) {
                myplot <- myplot +
                    private$get_facet_grid()
            }

            myplot
        },

        #' @description Returns the data tibble.
        get_data = function() {
            private$data
        },

        #' @description Sets the data tibble.
        #' @param data tibble.
        set_data = function(data) {
            private$data <- data
            invisible(self)
        },

        #' @description Plots a directed acyclic graph for the Lgc object.
        #' @export
        #' @param data_syntax List of characters. Each character represents
        #' syntax to simulate data for a separate group.
        #' @param sample_size Vector of integers. Must be of length or the
        #' same length as data_syntax.
        #' @param replications Integer. Number of replications.
        #' @param ... Additional arguments.
        #' @importFrom lavaan simulateData
        #' @import tidyverse
        power_analysis = function(
        data_syntax,
        sample_size = 100L,
        replications = 500L,
        ...
        ) {

            data_orig <- private$data
            sem_orig <- private$sem_obj

            n_groups <- length(private$group_labels)

            if (length(sample_size) != 1L && length(sample_size) != n_groups) {
                stop("sample_size must be of length 1 or the same length as data_syntax")
            }

            if (length(sample_size == 1L)) {
                sample_size <- rep_len(sample_size, n_groups)
            }

            pt <- lavaanify(data_syntax, ngroups = n_groups)

            get_data <- function() {
                simulateData(
                    pt,
                    group.label = private$group_labels,
                    sample.nobs = sample_size
                ) %>%
                    as_tibble() %>%
                    mutate(.group = private$group_labels[group])
            }

            run <- function(rep_index) {
                private$data <- get_data()
                self$estimate()

                tests <- self$tests_table(private$hypotheses)

                if (is.null(rownames(tests))) {
                    rownames(tests) <- paste0("hypothesis", 1:nrow(tests))
                } else {
                    indices <- which(rownames(tests) == "")
                    rownames(tests)[indices] <- paste0("hypothesis", indices)
                }

                tests$hypothesis <- rownames(tests)
                tests <- tests %>%
                    as_tibble() %>%
                    rename(pvalue = "Pr(>Chisq)") %>%
                    mutate(rep_index = rep_index)

                estimates <- self$get_estimates() %>%
                    mutate(rep_index = rep_index)

                list(tests = tests, estimates = estimates)
            }

            results <- lapply(1:replications, function(rep_index) {
                tryCatch({
                    results <- run(rep_index)
                    list(
                        tests = results$tests,
                        estimates = results$estimates
                    )
                }, warning = function(w) {
                    results <- run(rep_index)
                    list(
                        tests = results$tests %>% mutate(warning = w$message),
                        estimates = results$estimates %>% mutate(warning = w$message)
                    )
                }, error = function(e) {
                    list(
                        tests <- tibble(error = e$message, rep_index = rep_index),
                        estimates <- tibble(error = e$message, rep_index = rep_index)
                    )
                })
            })

            tests <- lapply(results, `[[`, 1) %>%
                bind_rows()

            tests_aggregated <- tests %>%
                mutate(hypothesis = factor(hypothesis)) %>%
                group_by(hypothesis) %>%
                summarise(power = mean(pvalue < 0.05, na.rm = T)) %>%
                ungroup()

            estimates <- lapply(results, `[[`, 2) %>%
                bind_rows() %>%
                filter(free) %>%
                mutate(nice_label = private$get_labels(label, format = "text"))

            estimates_aggregated <- estimates %>%
                select(parameter = nice_label, label, est, est, group, se) %>%
                mutate(pvalue = 2*(1-pnorm(abs(est/se)))) %>%
                group_by(group, parameter) %>%
                summarise(
                    label = first(label),
                    mean = mean(est, na.rm = T),
                    sd = sd(est, na.rm = T),
                    pctNA = mean(is.na(est)),
                    power = mean(pvalue < 0.05, na.rm = T),
                    pct2.5 = quantile(est, 0.025, na.rm = T),
                    pct25 = quantile(est, 0.25, na.rm = T),
                    median = median(est, na.rm = T),
                    pct75 = quantile(est, 0.75, na.rm = T),
                    pct97.5 = quantile(est, 0.975, na.rm = T)
                ) %>%
                ungroup()

            private$data <- data_orig
            private$sem_obj <- sem_orig

            list(
                tests = tests,
                tests_aggregated = tests_aggregated,
                estimates = estimates,
                estimates_aggregated = estimates_aggregated
            )
        },

        #' @title Prints the reliabilities of the indicators of latent variables.
        #' @export
        reliabilities = function() {
            cat("This is work in progress.\n")
        }
    )
)


#' Wrapper method for lgc$print()
#' @export
#' @method print Lgc
#' @param x Object of class Lgc.
#' @param ... Arguments passed down to lgc$print().
print.Lgc <- function(x, ...) {
    x$print(...)
}

#' Wrapper method for lgc$plot()
#' @export
#' @method plot Lgc
#' @param x Object of class Lgc.
#' @param ... Arguments passed down to lgc$plot().
plot.Lgc <- function(x, ...) {
    x$plot(...)
}

#' Wrapper method for lgc$summary()
#' @export
#' @method summary Lgc
#' @param object Object of class Lgc.
#' @param ... Arguments passed down to lgc$summary().
summary.Lgc <- function(object, ...) {
    object$summary(...)
}

#' @rdname lgc
#' @export
lgc <- function(...) {
    Lgc$new()$specify(...)
}

#' @rdname lgc
#' @export
lgc_dummy <- function(...) {
    Lgc$new()$specify_dummy(...)
}

#' @title Tests a hypothesis.
#' @export
#' @param lgc Object of class Lgc.
#' @param hypothesis Object of class Hypothesis.
test_hypothesis <- function(lgc, hypothesis) {
    lgc$test(hypothesis)
}

#' @title Prints the reliabilities of the indicators of latent variables.
#' @export
#' @param lgc Object of class Lgc.
reliabilities <- function(lgc) {
    lgc$reliabilities()
}

#' Compares the lavaan objects contained in Lgc objects.
#' @export
#' @method anova Lgc
#' @param object Object of class Lgc.
#' @param ... Additional arguments passed down to
#' anova(lavaan_object, ...).
#' @importFrom lavaan lavTestLRT
anova.Lgc <- function(object, ...) {

    # Mostly copied from the lavaan package.
    # Thanks, Yves!!

    object <- object$get_sem_object

    mcall <- match.call(expand.dots = TRUE)
    dots <- list(...)

    SB.classic <- TRUE; SB.H0 <- FALSE

    arg.names <- names(dots)
    arg.idx <- which(nchar(arg.names) > 0L)
    if(length(arg.idx) > 0L) {
        if(!is.null(dots$SB.classic))
            SB.classic <- dots$SB.classic
        if(!is.null(dots$SB.H0))
            SB.H0 <- dots$SB.H0
        dots <- dots[-arg.idx]
    }

    modp <- sapply(dots, inherits, "Lgc")
    mods <- c(list(object), dots[modp])
    NAMES <- sapply(as.list(mcall)[c(FALSE, TRUE, modp)], deparse)
    dots <- lapply(dots, function(x) if (inherits(x, "Lgc")) x$get_sem_object else x)

    ans <- do.call("lavTestLRT", c(list(object = object,
                                        SB.classic = SB.classic, SB.H0 = SB.H0,
                                        model.names = NAMES), dots))
    ans
}

