#' Semnova class
#'
#' @description Some description
#' @rdname semnova
#' @export
#' @importFrom R6 R6Class
#' @details \code{semnova()} is a wrapper for the R6 class constructor
#' \code{Semnova$new()}. See \code{Semnova$new()} below for a list of arguments.
#'
Semnova <- R6::R6Class(
    "Semnova",
    inherit = Lgc,
    private = list(
        withins = list(),
        within_design = NULL,
        betweens = list(),
        between_design = NULL,
        manifest_covariates = character(),
        latent_covariates = list(),
        covariates_description = list(),
        covariates_design = NULL,
        indicators = list(),
        id = character(),
        dv = character(),
        contrasts_arg = list(),
        resid_cov = list(),
        equal_resid_cov = list(),
        sphericity = list(),
        normalize_contrasts = FALSE,
        input_semnova = list(),
        model_info = NULL,
        data_original = NULL,

        #' @keywords internal
        get_term_names = function(idesign, idata) {
            if (attr(terms(idesign), "intercept")) {
                c("Intercept", labels(terms(idesign)))
            } else {
                labels(terms(idesign))
            }

        },

        #' @keywords internal
        #' @import tidyverse
        get_facet_formula = function() {
            between <- names(private$betweens)
            n_groups <- length(between)

            cols <- seq(1, ceiling(n_groups/2))
            rows <- if (n_groups == 1L) integer() else seq(ceiling(n_groups/2)+1, n_groups)

            cols <- between[cols]
            rows <- between[rows]

            cols <- sapply(cols, function(col)
                paste0('private$match_group(group, "', col, '")')) %>%
                paste0(collapse = " + ")

            if (length(rows) == 0L) {
                rows <- "."
            } else {
                rows <- sapply(rows, function(row)
                    paste0('private$match_group(group, "', row, '")')) %>%
                    paste0(collapse = " + ")
            }

            form <- paste0(rows, "~", cols)
            as.formula(form)
        },

        #' @keywords internal
        #' @import tidyverse
        match_group = function(group, return_group) {
            idata <- private$parse_between_design(
                betweens = private$betweens,
                contrasts_arg = private$contrasts_arg,
                normalize_contrasts = private$normalize_contrasts
            )$idata

            group_indices <- sapply(group, function(g) which(g == private$group_labels))
            groups <- idata[group_indices,,drop = F]
            groups <- lapply(1:ncol(groups), function(x) paste0(colnames(idata)[x], ": ", groups[,x]))
            names(groups) <- colnames(idata)

            groups[[return_group]]
        },

        #' @keywords internal
        extract_dep_var = function(form) {
            if (attr(terms(form), "response") == 0L) {
                return(NULL)
            }

            response <- attr(terms(form), "variables")[[2]]

            if(is.call(response)) {
                ## multivariate
                response <- as.list(response)
                if (response[1] != "cbind") {
                    return(NULL)
                }
                response <- response[-1]
                response <- sapply(response, as.character)
                return(response)
            } else {
                ## univariate
                return(as.character(response))
            }
        },

        #' @keywords internal
        parse_sphericity = function(sphericity, idesign, b_matrix) {
            model_terms <- private$get_term_names(idesign)
            factors <- attr(terms(idesign), "factors")

            if (is.logical(sphericity) && sphericity) {
                return(list(1:length(model_terms)))
            } else if (is.logical(sphericity) && !sphericity) {
                return(list())
            }

            if (is_formula(sphericity)) {
                lhs <- as.logical(private$extract_dep_var(sphericity))
                rhs <- private$get_term_names(sphericity)
            } else if (is.list(sphericity)) {

                lhs <- FALSE
                rhs <- sapply(sphericity, function(constraint) {
                    if ((constraint == "Intercept") && ("Intercept" %in% model_terms)) {
                        return("Intercept")
                    }

                    indices <-
                        sapply(rownames(factors), function(x)
                            x %in% constraint)
                    indices <- as.integer(indices)

                    index <-
                        which(apply(factors, 2, function(factor) {
                            all(sapply(1:length(indices), function(x)
                                indices[x] == factor[x]))
                        }))

                    labels(terms(idesign))[index]
                })
            } else {
                stop("I don't understand the sphericity argument.")
            }

            indices <-
                lapply(1:length(model_terms), function(model_term_index) {
                    model_term <- model_terms[model_term_index]

                    if (any(rhs == model_term) && !lhs) {
                        which(model_term_index - 1 == attr(b_matrix, "assign"))
                    } else if (!any(rhs == model_term) && lhs) {
                        which(model_term_index - 1 == attr(b_matrix, "assign"))
                    } else {
                        NULL
                    }
                })

            indices <- indices[sapply(indices, function(x)
                ! is.null(x) & length(x) > 1L)]

            indices
        },

        #' @keywords internal
        validate_specify = function(call, env = parent.env(environment())) {
            is_missing <- function(name) {
                is.null(languageEl(call, name))
            }
        },

        #' @keywords internal
        validate_new = function() {
            # validate: 2. covariates ------------------------------------------

            if (is.null(covariates)) {
                private$covariates <- character()
                private$covariates_mmodel <- list()
            } else if (is.character(covariates)) {
                # covariates is a character vector (manifest variables)
                private$covariates <-
                    paste0(".covariate", 1:length(covariates))
                private$covariates_mmodel <- list(covariates)
                names(private$covariates_mmodel) <- private$covariates

            } else if (is.list(covariates)) {
                # covariates is a list (latent variables)

                private$covariates_mmodel <- covariates

                if (is.null(names(private$covariates_mmodel))) {
                    names(private$covariates_mmodel) <-
                        paste0(".covariate", 1:length(private$covariates_mmodel))
                }

                private$covariates <- names(private$covariates_mmodel)
            }

            # validate: 3. groups ----------------------------------------------

            if (is.null(group)) {
                data$.group <- "1"
                private$group <- ".group"
                private$group_labels <- "1"
            } else {
                private$group <- group
                private$group_labels <- unique(data[[group]])
            }

            private$group_weights <- group_weights

            # validate: 4. pis -------------------------------------------------

            private$pis <- paste0("pi", 1:length(private$etas))

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

            # validate: 7. everything else -------------------------------------

            private$c_matrix_within    <- c_matrix_within
            private$resid_cov          <- resid_cov
            private$equal_resid_cov    <- equal_resid_cov
            private$invariance_within  <- invariance_within
            private$c_matrix_between   <- c_matrix_between
            private$invariance_between <- invariance_between
            private$lv_scaling         <- lv_scaling
            private$hypotheses         <- hypotheses
            private$append             <- append
        },

        #' @keywords internal
        expand_data_covariates = function(
            data,
            manifest_covariates,
            latent_covariates,
            contrasts_arg,
            normalize_contrasts
        ) {
            private$parse_covariates(
                manifest_covariates = manifest_covariates,
                latent_covariates = latent_covariates,
                data = data,
                contrasts_arg = contrasts_arg,
                normalize_contrasts = normalize_contrasts
            )
        },

        #' @keywords internal
        get_covariates_idata = function(description) {
            lapply(description, function(desc) if (is.numeric(desc)) 0 else desc) %>%
                (function(x) do.call(expand.grid, x))
        },

        #' @keywords internal
        get_covariates_description = function(data, manifest_covariates) {
            description <- lapply(manifest_covariates, function(covariate) {
                if (is.numeric(data[[covariate]])) {
                    numeric()
                } else if (is.character(data[[covariate]]) || is.factor(data[[covariate]])) {
                    unique(data[[covariate]])
                }
            })
            names(description) <- manifest_covariates
            description
        },

        #' @keywords internal
        parse_covariates_with_description = function(
            manifest_covariates,
            latent_covariates,
            description,
            contrasts_arg,
            normalize_contrasts
        ) {
            data <- private$get_covariates_idata(description)
            covariates_design <- private$parse_covariates(
                manifest_covariates = manifest_covariates,
                latent_covariates = latent_covariates,
                data = data,
                contrasts_arg = contrasts_arg,
                normalize_contrasts
            )
            covariates_design$data <- NULL
            covariates_design
        },

        #' @keywords internal
        parse_covariates_old = function(
            manifest_covariates = NULL,
            latent_covariates = NULL,
            data = NULL,
            contrasts_arg = list(),
            normalize_contrasts
        ) {

            if (is.null(latent_covariates) && is.null(manifest_covariates)) {
                return(list(
                    data = NULL,
                    covariates_mmodel = NULL,
                    effect_names_covariates <- character(),
                    effect_indices_covariates <- integer()
                ))
            }

            covariates_mmodel <- list()
            effect_names_covariates <- "Intercept"
            effect_indices_covariates <- 0

            if (!is.null(manifest_covariates)) {
                contrasts_arg_covariates <- lapply(manifest_covariates, function(x) {
                    if (!is.null(contrasts_arg[[x]])) {
                        contrasts_arg[[x]]
                    } else {
                        if (is.factor(data[[x]]) || is.character(data[[x]])) {
                            "contr.treatment"
                        }
                    }
                })
                names(contrasts_arg_covariates) <- manifest_covariates
                contrasts_arg_covariates <- contrasts_arg_covariates[!sapply(contrasts_arg_covariates, is.null)]

                design_covariates <- paste0("~", paste0(manifest_covariates, collapse = "*")) %>% as.formula()

                opts_old <- options(na.action='na.pass')
                b_matrix_covariates <- model.matrix(
                    design_covariates,
                    data,
                    contrasts_arg_covariates
                )
                options(opts_old)

                effect_names_covariates <- c("Intercept", attr(terms(design_covariates), "term.labels"))
                effect_indices_covariates <- attr(b_matrix_covariates, "assign")

                data <- data %>%
                    select_at(names(data)[!(names(data) %in% manifest_covariates)])

                # b_matrix_covariates <- b_matrix_covariates[,-1,drop=F] %>%
                #     as_tibble(.name_repair = "universal")

                b_matrix_covariates <- b_matrix_covariates[,-1,drop=F] %>%
                    as_tibble()
                names(b_matrix_covariates) <-
                    vctrs::vec_as_names(names(b_matrix_covariates),
                                        repair = "universal", quiet = T)

                data <- bind_cols(data, b_matrix_covariates)

                covariates_mmodel <- as.list(names(b_matrix_covariates))
                names(covariates_mmodel) <- paste0(".covariate", 1:length(covariates_mmodel))

            }

            if (!is.null(latent_covariates)) {

                last_index <- length(covariates_mmodel)

                if (is.null(names(latent_covariates))) {
                    names(latent_covariates) <-paste0(
                        ".covariate",
                        seq(last_index + 1, last_index + length(latent_covariates))
                    )
                }

                empty_names_indices <- which(names(latent_covariates) == "")
                names(latent_covariates)[empty_names_indices] <- paste0(".covariate", empty_names_indices + last_index)

                covariates_mmodel <- c(covariates_mmodel, latent_covariates)

                effect_names_covariates <- c(effect_names_covariates, names(latent_covariates))
                effect_indices_covariates <- c(
                    effect_indices_covariates,
                    seq(max(c(0, effect_indices_covariates)) + 1, max(c(0, effect_indices_covariates)) + 1 + length(latent_covariates) - 1)
                )
            }

            list(
                data = data,
                idata = idata_within,
                idesign = idesign_within,
                covariates_mmodel = covariates_mmodel,
                effect_names_covariates = effect_names_covariates,
                effect_indices_covariates = effect_indices_covariates
            )
        },

        #' @keywords internal
        parse_covariates = function(
            manifest_covariates = NULL,
            latent_covariates = NULL,
            data = NULL,
            contrasts_arg = list(),
            normalize_contrasts
        ) {

            if (is.null(latent_covariates) && is.null(manifest_covariates)) {
                c_matrix_covariates <- matrix(1, ncol = 1, nrow = 1)
                dimnames(c_matrix_covariates) <- list("(Intercept)", "dummy")
                b_matrix_covariates <- solve(c_matrix_covariates)

                effect_names_covariates <- "Intercept"
                effect_indices_covariates <- 0
                attr(b_matrix_covariates, "assign") <- 0
                return(list(
                    data = data,
                    idata = NULL,
                    idesign = ~1,
                    b_matrix_covariates = b_matrix_covariates,
                    covariates_mmodel = NULL,
                    effect_names_covariates = effect_names_covariates,
                    effect_indices_covariates = effect_indices_covariates
                ))
                # return(list(
                #     data = NULL,
                #     covariates_mmodel = NULL,
                #     effect_names_covariates = character(),
                #     effect_indices_covariates = integer()
                # ))
            }

            covariates_mmodel <- list()
            effect_names_covariates <- "Intercept"
            effect_indices_covariates <- 0

            if (!is.null(latent_covariates)) {
                for (index in 1:length(latent_covariates)) {
                    covariate_name <- names(latent_covariates)[index]
                    data[[covariate_name]] <- NA_real_
                }
            }

            contrasts_arg_covariates <- lapply(manifest_covariates, function(x) {
                if (!is.null(contrasts_arg[[x]])) {
                    contrasts_arg[[x]]
                } else {
                    if (is.factor(data[[x]]) || is.character(data[[x]])) {
                        "contr.treatment"
                    }
                }
            })
            names(contrasts_arg_covariates) <- manifest_covariates
            contrasts_arg_covariates <- contrasts_arg_covariates[!sapply(contrasts_arg_covariates, is.null)]

            manifest_design <- paste0(manifest_covariates, collapse = " * ")
            latent_design <- paste0(names(latent_covariates), collapse = " + ")
            if (!is.null(manifest_covariates) && !is.null(latent_covariates)) {
                latent_design <- paste0(" + ", latent_design)
            }

            design_covariates <- paste0(
                "~",
                manifest_design,
                latent_design
            ) %>% as.formula()

            opts_old <- options(na.action='na.pass')
            b_matrix_covariates <- model.matrix(
                design_covariates,
                data,
                contrasts_arg_covariates
            )
            options(opts_old)

            effect_names_covariates <- c("Intercept", attr(terms(design_covariates), "term.labels"))
            effect_indices_covariates <- attr(b_matrix_covariates, "assign")

            data <- data %>%
                select_at(names(data)[!(names(data) %in% c(manifest_covariates, names(latent_covariates)))])

            # b_matrix_covariates <- b_matrix_covariates[,-1,drop=F] %>%
            #     as_tibble(.name_repair = "universal")

            b_matrix_covariates_tbl <- b_matrix_covariates[,-1,drop=F] %>%
                as_tibble()
            b_matrix_covariates_tbl <- b_matrix_covariates_tbl[,!(names(b_matrix_covariates_tbl) %in% names(latent_covariates))]
            b_matrix_names <- names(b_matrix_covariates_tbl)
            b_matrix_names <- b_matrix_names[!(b_matrix_names %in% names(latent_covariates))]

            names(b_matrix_covariates_tbl) <- vctrs::vec_as_names(
                b_matrix_names,
                repair = "universal", quiet = T
            )

            data <- bind_cols(data, b_matrix_covariates_tbl)

            covariates_mmodel <- as.list(names(b_matrix_covariates_tbl))
            names(covariates_mmodel) <- paste0(".covariate", 1:length(covariates_mmodel))

            if (!is.null(latent_covariates)) {
                last_index <- length(covariates_mmodel)

                if (is.null(names(latent_covariates))) {
                    names(latent_covariates) <-paste0(
                        ".covariate",
                        seq(last_index + 1, last_index + length(latent_covariates))
                    )
                }

                empty_names_indices <- which(names(latent_covariates) == "")
                names(latent_covariates)[empty_names_indices] <- paste0(".covariate", empty_names_indices + last_index)

                covariates_mmodel <- c(covariates_mmodel, latent_covariates)



                # effect_names_covariates <- c(effect_names_covariates, names(latent_covariates))
                # effect_indices_covariates <- c(
                #     effect_indices_covariates,
                #     seq(max(c(0, effect_indices_covariates)) + 1, max(c(0, effect_indices_covariates)) + 1 + length(latent_covariates) - 1)
                # )
            }

            list(
                data = data,
                idata = NULL,
                idesign = design_covariates,
                b_matrix_covariates = b_matrix_covariates,
                covariates_mmodel = covariates_mmodel,
                effect_names_covariates = effect_names_covariates,
                effect_indices_covariates = effect_indices_covariates
            )
        },

        #' @keywords internal
        parse_within_design = function(withins, contrasts_arg, normalize_contrasts) {
            idata_within <- do.call(expand.grid, withins)
            idesign_within <- as.formula(paste0("~", paste0(names(withins), collapse = "*")))

            if (all(sapply(withins, length) == 1L)) {
                c_matrix_within <- matrix(1, ncol = 1, nrow = 1)
                dimnames(c_matrix_within) <- list("(Intercept)", "dv")
                b_matrix_within <- solve(c_matrix_within)

                effect_names_within <- "Intercept"
                effect_indices_within <- 0
                attr(b_matrix_within, "assign") <- 0
            } else {
                contrasts_arg_within <- lapply(names(withins), function(x) {
                    if (!is.null(contrasts_arg[[x]])) {
                        contrasts_arg[[x]]
                    } else {
                        "contr.poly"
                    }
                })
                names(contrasts_arg_within) <- names(withins)

                b_matrix_within <- model.matrix(
                    idesign_within,
                    idata_within,
                    contrasts.arg = contrasts_arg_within
                )

                if (normalize_contrasts) {
                    for (col in 1:ncol(b_matrix_within)) {
                        b_matrix_within[,col] <- b_matrix_within[,col] / sqrt(sum(b_matrix_within[,col]^2))
                    }
                }

                c_matrix_within <- solve(b_matrix_within)

                effect_names_within <- c("Intercept", attr(terms(idesign_within), "term.labels"))
                effect_indices_within <- attr(b_matrix_within, "assign")
            }

            list(
                idata = idata_within,
                idesign = idesign_within,
                c_matrix = c_matrix_within,
                b_matrix = b_matrix_within,
                effect_names = effect_names_within,
                effect_indices = effect_indices_within
            )
        },

        #' @keywords internal
        parse_mmodel_eta = function(indicators, idata_within) {
            indicators_levels <- unlist(indicators)

            etas_mmodel <- apply(idata_within, 1, paste0, collapse = "_") %>%
                (function(etas) lapply(etas, function(eta) paste0(".", indicators_levels, "_", eta)))

            etas <- paste0(".eta", 1:nrow(idata_within))
            names(etas_mmodel) <- etas

            etas_mmodel
        },

        #' @keywords internal
        parse_between_design = function(
            betweens,
            contrasts_arg,
            normalize_contrasts
        ) {
            idata_between <- do.call(expand.grid, betweens)
            idesign_between <- as.formula(paste0("~", paste0(names(betweens), collapse = "*")))

            if (all(sapply(betweens, length) == 1L)) {
                c_matrix_between <- matrix(1, ncol = 1, nrow = 1)
                dimnames(c_matrix_between) <- list("(Intercept)", ".group")
                b_matrix_between <- solve(c_matrix_between)

                effect_names_between <- "Intercept"
                effect_indices_between <- 0
                attr(b_matrix_between, "assign") <- 0
            } else {
                contrasts_arg_between <- lapply(names(betweens), function(x) {
                    if (!is.null(contrasts_arg[[x]])) {
                        contrasts_arg[[x]]
                    } else {
                        "contr.treatment"
                    }
                })
                names(contrasts_arg_between) <- names(betweens)

                b_matrix_between <- model.matrix(
                    idesign_between,
                    idata_between,
                    contrasts.arg = contrasts_arg_between
                )

                if (normalize_contrasts) {
                    for (col in 1:ncol(b_matrix_between)) {
                        b_matrix_between[,col] <- b_matrix_between[,col] /
                            sqrt(sum(b_matrix_between[,col]^2))
                    }
                }

                c_matrix_between <- solve(b_matrix_between)

                effect_names_between <- c(
                    "Intercept",
                    attr(terms(idesign_between), "term.labels")
                )
                effect_indices_between <- attr(b_matrix_between, "assign")
            }

            list(
                idata = idata_between,
                idesign = idesign_between,
                c_matrix = c_matrix_between,
                b_matrix = b_matrix_between,
                effect_names = effect_names_between,
                effect_indices = effect_indices_between
            )
        },

        #' @keywords internal
        parse_hypotheses = function(within_design, between_design, covariates_design) {
            # M matrices

            Ms <- lapply(unique(within_design$effect_indices), function(effect_index) {

                effect <- within_design$effect_names[effect_index]

                lapply(1:length(within_design$effect_indices), function(col_index) {
                    my_col <- within_design$effect_indices[col_index]
                    if (my_col == effect_index) as.integer((1:nrow(within_design$c_matrix)) == col_index) else NULL
                }) %>% (function(cols) do.call(cbind, cols))
            })

            names(Ms) <- within_design$effect_names

            # L matrices

            Ls <- lapply(unique(between_design$effect_indices), function(effect_index) {
                effect <- between_design$effect_indices[effect_index]

                between_design$c_matrix[between_design$effect_indices == effect_index,,drop = F]
            })

            names(Ls) <- between_design$effect_names


            # P matrices

            if (length(covariates_design$covariates_mmodel) > 0L) {
                Ps <- lapply(unique(covariates_design$effect_indices), function(effect_index) {

                    effect <- covariates_design$effect_names[effect_index]

                    lapply(1:length(covariates_design$effect_indices), function(col_index) {
                        my_col <- covariates_design$effect_indices[col_index]
                        if (my_col == effect_index) as.integer((1:length(covariates_design$effect_indices)) == col_index) else NULL
                    }) %>% (function(cols) do.call(rbind, cols))
                })

                names(Ps) <- covariates_design$effect_names

            } else {
                Ps <- list(matrix(1, ncol = 1, nrow = 1))
                names(Ps) <- c("Intercept")
            }

            # create hypotheses

            hypotheses <- list()

            for (P_index in 1:length(Ps)) {
                P_name <- names(Ps)[P_index]
                P_matrix <- Ps[[P_index]]

                for (L_index in 1:length(Ls)) {
                    L_name <- names(Ls)[L_index]
                    L_matrix <- Ls[[L_index]]

                    for (M_index in 1:length(Ms)) {
                        M_name <- names(Ms)[M_index]
                        M_matrix <- Ms[[M_index]]

                        hypothesis_name <- c(M_name, L_name, P_name)

                        hypothesis_name <-
                            hypothesis_name[hypothesis_name != "Intercept"]

                        if (length(hypothesis_name) == 0L) {
                            hypothesis_name <- "Intercept"
                        } else {
                            hypothesis_name <- paste0(hypothesis_name, collapse = ":")
                        }

                        hypotheses <- c(
                            hypotheses,
                            Hypothesis$new(
                                M = M_matrix,
                                L = L_matrix,
                                P = P_matrix,
                                description = hypothesis_name
                            )
                        )
                    }
                }
            }

            hypotheses

        },

        #' @keywords internal
        specify_internal = function(
            withins = list(A = "A1"),      # named list: withins = list(A = c("A1", "A2", "A2"))
            betweens = list(G1 = "G1_1"),  # named list: betweens = list(group = c("g1", "g2", "g3"))
            manifest_covariates = NULL,    # character vector: manifest_covariates = c("covariate3")
            covariates_description = NULL, # named list: covariates_description = list(C1 = numeric(), group = c("g1", "g2"))
            latent_covariates = NULL,      # named list: latent_covariates = list(cov1 = c("covariate1", "covariate2"))
            indicators = "Y1",             # named list: indicators = list(indicators = c("Y1", "Y2", "Y3"))
            id = "id",                     # character vector: id = c("id"),
            dv = "dv",                     # character: "dv"
            contrasts_arg = list(),        # named list: contrasts_arg = list(A = "contr.poly", group = "contr.sum"),
            resid_cov = list(),            # named list: resid_cov = list("Y1", "Y2", "Y3")
            equal_resid_cov = list(),      # named list: equal_resid_cov = list("Y1", "Y2", "Y3")
            sphericity = list(),           # UNnamed list or formula: sphericity = list("A")
            normalize_contrasts = FALSE,   # logical: normalize_contrasts = FALSE
            data = NULL,
            ...
        ) {

            private$withins <- withins
            private$betweens <- betweens
            private$manifest_covariates <- manifest_covariates
            private$covariates_description <- covariates_description
            private$latent_covariates <- latent_covariates
            private$indicators <- indicators
            private$id <- id
            private$dv <- dv
            private$contrasts_arg <- contrasts_arg
            private$resid_cov <- resid_cov
            private$equal_resid_cov <- equal_resid_cov
            private$sphericity <- sphericity
            private$normalize_contrasts <- normalize_contrasts

            # initialize: 1. create within contrast matrix ---------------------

            within_design <- private$parse_within_design(
                withins = withins,
                contrasts_arg = contrasts_arg,
                normalize_contrasts = normalize_contrasts
            )

            private$within_design <- within_design

            # initialize: 2. create eta mmodel ---------------------------------

            etas_mmodel <- private$parse_mmodel_eta(
                indicators = indicators,
                idata_within = within_design$idata
            )

            # initialize: 3. create between contrast matrix --------------------

            between_design <- private$parse_between_design(
                betweens = betweens,
                contrasts_arg = contrasts_arg,
                normalize_contrasts = normalize_contrasts
            )

            private$between_design <- between_design


            # initialize: 7. create group labels -------------------------------

            group <- private$get_data_name(what = "group")

            group_labels <- apply(between_design$idata, 1, paste0, collapse = "_")

            # initialize: 8. parse covariates ----------------------------------

            covariates_design <- private$parse_covariates_with_description(
                description = covariates_description,
                manifest_covariates = manifest_covariates,
                latent_covariates = latent_covariates,
                contrasts_arg = contrasts_arg,
                normalize_contrasts = normalize_contrasts
            )

            private$covariates_design <- covariates_design

            # initialize: 9. parse resid_cov -----------------------------------

            if (!is.null(resid_cov) && length(resid_cov) > 0L) {

                etas <- apply(within_design$idata, 1, paste0, collapse = "_")
                resid_cov <- lapply(1:length(resid_cov), function(resid_cov_index) {
                    resid_indicators <- resid_cov[[resid_cov_index]]

                    lapply(resid_indicators, function(resid_indicator) {
                        paste0(".", resid_indicator, "_", etas)
                    }) %>% (function(x) do.call(c, x))

                })

            }

            if (!is.null(equal_resid_cov) && length(equal_resid_cov) > 0L) {

                etas <- apply(within_design$idata, 1, paste0, collapse = "_")
                equal_resid_cov <- lapply(1:length(equal_resid_cov), function(resid_cov_index) {
                    resid_indicators <- equal_resid_cov[[resid_cov_index]]

                    lapply(resid_indicators, function(resid_indicator) {
                        paste0(".", resid_indicator, "_", etas)
                    }) %>% (function(x) do.call(c, x))

                })

            }

            # initialize: 11. main and interaction effects ---------------------

            hypotheses <- private$parse_hypotheses(
                within_design,
                between_design,
                covariates_design
            )

            # initialize: 12. average effects ----------------------------------


            # initialize: 13. parse sphericity formula -------------------------

            sphericity <- private$parse_sphericity(
                sphericity,
                within_design$idesign,
                within_design$b_matrix
            )

            super$specify(
                data = data,
                etas = etas_mmodel,
                covariates = covariates_design$covariates_mmodel,
                group = group,
                group_labels = group_labels,
                c_matrix_within = within_design$c_matrix,
                sphericity = sphericity,
                hypotheses = hypotheses,
                resid_cov = resid_cov,
                equal_resid_cov = equal_resid_cov,
                ...
            )

            private$fill_model_info()

            invisible(self)
        },

        .get_model_frame_old = function() {
            within_data <- lapply(private$data$id, function(id) {
                private$within_design$idata %>%
                    as_tibble() %>%
                    mutate(!!private$id := id)
            }) %>%
                bind_rows()

            between_data <- private$between_design$idata %>%
                as_tibble() %>%
                mutate(.group = private$group_labels) %>%
                inner_join(
                    private$data %>% select_at(c("id", ".group")),
                    by = ".group"
                ) %>%
                select(-all_of(".group"))

            manifest_covariates <- private$covariates[!sapply(private$covariates, self$is_latent)]
            latent_covariates <- private$covariates[sapply(private$covariates, self$is_latent)]

            fs_covariates <- predict(self$get_sem_object) %>%
                (function(x) if(!is.list(x)) list(x) else x) %>%
                lapply(function(fs) {
                    fs[,latent_covariates,drop =F]
                })

            covariates_data <- private$data %>%
                mutate(case.idx = row_number()) %>%
                distinct(case.idx, .keep_all=T) %>%
                select_at(c(".group", "id", "case.idx", manifest_covariates)) %>%
                group_by(.group) %>%
                group_modify(function(tbl, desc) {
                    group_index <- which(desc$.group == private$group_labels)
                    lav_data <- private$sem_obj@Data
                    ids <- lav_data@case.idx[[group_index]]
                    fs <- fs_covariates[[group_index]]
                    fs <- as_tibble(fs) %>%
                        mutate(case.idx = ids)
                    full_join(tbl, fs, by = "case.idx")
                }) %>%
                ungroup() %>%
                select(-all_of(c(".group", "case.idx")))

            within_data %>%
                left_join(
                    between_data,
                    by = "id"
                ) %>%
                left_join (
                    covariates_data,
                    by = "id"
                )
        },

        .get_model_frame = function() {
            dat <- private$data_original %>%
                select_at(c(
                    private$id,
                    names(private$withins),
                    names(private$betweens)
                )) %>%
                distinct()

            manifest_covariates <- private$covariates[!as.logical(sapply(private$covariates, self$is_latent))]
            latent_covariates <- private$covariates[as.logical(sapply(private$covariates, self$is_latent))]

            fs_covariates <- predict(self$get_sem_object) %>%
                (function(x) if(!is.list(x)) list(x) else x) %>%
                lapply(function(fs) {
                    fs[,latent_covariates,drop =F]
                })

            covariates_data <- private$data %>%
                mutate(case.idx = row_number()) %>%
                distinct(case.idx, .keep_all=T) %>%
                select_at(c(".group", private$id, "case.idx", manifest_covariates)) %>%
                group_by(.group) %>%
                group_modify(function(tbl, desc) {
                    group_index <- which(desc$.group == private$group_labels)
                    lav_data <- private$sem_obj@Data
                    ids <- lav_data@case.idx[[group_index]]
                    fs <- fs_covariates[[group_index]]
                    fs <- as_tibble(fs) %>%
                        mutate(case.idx = ids)
                    full_join(tbl, fs, by = "case.idx")
                }) %>%
                ungroup() %>%
                select(-all_of(c(".group", "case.idx")))

            dat %>%
                left_join (
                    covariates_data,
                    by = private$id
                ) %>%
                ungroup()
        },

        #' @import tidyverse
        #' @importFrom Deriv Simplify
        fill_model_info = function() {
            private$model_info <- private$.get_model_info()
        },

        #' @import tidyverse
        #' @importFrom Deriv Simplify
        .get_model_info = function(dat = NULL) {

            if (is.null(dat)) {
                dat <- private$.get_model_frame()
            }
            if (!(all(sapply(private$id, `%in%`, names(dat))))) {
                for (id in private$id) {
                    dat <- dat %>%
                        mutate(!!id := row_number())
                }
            }

            within_conditions <- private$within_design$idata %>%
                apply(1, paste0, collapse = "_")

            within_data <- dat %>%
                unite(".within_condition", all_of(names(private$withins))) %>%
                `[[`(".within_condition") %>%
                sapply(function(x) which(x == within_conditions)) %>%
                (function(x) private$within_design$b_matrix[x,,drop=F]) %>%
                bind_cols(dat) %>%
                select(all_of(c(private$id, colnames(private$within_design$b_matrix)))) %>%
                distinct()

            manifest_covariates <- private$covariates[!as.logical(sapply(private$covariates, self$is_latent))]
            latent_covariates <- private$covariates[as.logical(sapply(private$covariates, self$is_latent))]

            between_conditions <- private$between_design$idata %>%
                apply(1, paste0, collapse = "_")

            between_data <- dat %>%
                unite(".between_condition", all_of(names(private$betweens))) %>%
                `[[`(".between_condition") %>%
                sapply(function(x) which(x == between_conditions)) %>%
                (function(x) private$between_design$b_matrix[x,,drop=F]) %>%
                bind_cols(dat) %>%
                select_at(c(
                    private$id,
                    colnames(private$between_design$b_matrix)
                )) %>%
                distinct()

            between_data <- left_join(
                within_data,
                between_data,
                by = c(private$id, "(Intercept)")
            ) %>%
                select_at(names(between_data)) %>%
                select(all_of(c(private$id, "(Intercept)")), everything())

            covariates_data <- dat %>%
                select_at(c(
                    private$id,
                    manifest_covariates,
                    latent_covariates
                )) %>%
                distinct()

            covariates_data <- left_join(
                within_data,
                covariates_data,
                by = c(private$id)
            ) %>%
                select_at(names(covariates_data)) %>%
                mutate(`(Intercept)` = 1) %>%
                select(all_of(c(private$id, "(Intercept)")), everything())

            within_terms <- terms(private$within_design$idesign)
            between_terms <- terms(private$between_design$idesign)
            covariates_terms <- terms(private$covariates_design$idesign)

            within_term_labels <- attr(within_terms, "term.labels")
            between_term_labels <- attr(between_terms, "term.labels")
            covariates_term_labels <- attr(covariates_terms, "term.labels")

            within_b_matrix <- private$within_design$b_matrix
            between_b_matrix <- private$between_design$b_matrix
            covariates_b_matrix <- private$covariates_design$b_matrix_covariates

            within_nterms <- ncol(attr(within_terms, "factors"))
            between_nterms <- ncol(attr(between_terms, "factors"))
            covariates_nterms <- max(c(0, ncol(attr(covariates_terms, "factors"))))

            nvariables <- nrow(attr(within_terms, "factors")) +
                nrow(attr(between_terms, "factors")) +
                max(c(0, nrow(attr(covariates_terms, "factors"))))
            nterms <- (within_nterms+1) * (between_nterms+1) * (covariates_nterms+1)

            attr_variables <- c(
                rownames(attr(within_terms, "factors")),
                rownames(attr(between_terms, "factors")),
                rownames(attr(covariates_terms, "factors"))
            )

            attr_factors <- matrix(
                nrow = nvariables,
                ncol = nterms-1,
                dimnames = list(attr_variables, rep_len("", nterms-1))
            )

            attr_contrasts <- c(
                attr(private$within_design$b_matrix, "contrasts"),
                attr(private$between_design$b_matrix, "contrasts"),
                attr(private$covariates_design$b_matrix, "contrasts")
            )

            attr_formula <- c()
            attr_order <- vector("numeric", nterms)
            b_matrix <- within_data %>%
                select_at(private$id)
            attr_assign <- c()
            attr_term_labels <- c()
            attr_order <- c()
            counter <- 0
            for (covariates_effect_index in 0:covariates_nterms) {
                for (between_effect_index in 0:between_nterms) {
                    for (within_effect_index in 0:within_nterms) {

                        # covariates_effect_index <- 0
                        # between_effect_index <- 0
                        # within_effect_index <- 3

                        effect_index <-
                            covariates_effect_index * ((within_nterms+1)*(between_nterms+1)) +
                            between_effect_index * ((within_nterms+1)) +
                            within_effect_index

                        # print(effect_index)
                        # print("outer loop")
                        # print(covariates_effect_index)
                        # print(between_effect_index)
                        # print(within_effect_index)

                        effect_formula <- paste0(c(
                            if (within_effect_index == 0) NULL else private$within_design$effect_names[within_effect_index+1],
                            if (between_effect_index == 0) NULL else private$between_design$effect_names[between_effect_index+1],
                            if (covariates_effect_index == 0) NULL else private$covariates_design$effect_names[covariates_effect_index+1]
                        ), collapse = ":")

                        attr_formula <- c(
                            attr_formula,
                            if (effect_formula == "") "1" else effect_formula
                        )

                        if (any(c(covariates_effect_index,between_effect_index,within_effect_index)>0)) {
                            attr_factors[,effect_index] <-
                                rbind(
                                    # Within
                                    if (within_effect_index == 0) {
                                        matrix(0, nrow = nrow(attr(within_terms, "factors")))
                                    } else {
                                        attr(within_terms, "factors")[,within_effect_index,drop=F]
                                    },
                                    # Between
                                    if (between_effect_index == 0) {
                                        matrix(0, nrow = nrow(attr(between_terms, "factors")))
                                    } else {
                                        attr(between_terms, "factors")[,between_effect_index,drop=F]
                                    },
                                    # Covariates
                                    if (covariates_nterms == 0) {
                                        NULL
                                    } else if (covariates_effect_index == 0) {
                                        matrix(0, nrow = nrow(attr(covariates_terms, "factors")))
                                    } else {
                                        attr(covariates_terms, "factors")[,covariates_effect_index,drop=F]
                                    }
                                )
                            colnames(attr_factors)[effect_index] <- effect_formula
                            attr_term_labels <- c(attr_term_labels, effect_formula)
                            attr_order <- c(attr_order, sum(c(
                                if (within_effect_index == 0) 0 else attr(within_terms, "order")[within_effect_index],
                                if (between_effect_index == 0) 0 else attr(between_terms, "order")[between_effect_index],
                                if (covariates_effect_index == 0) 0 else attr(covariates_terms, "order")[covariates_effect_index]
                            )))
                        }

                        within_b_indices <- which(private$within_design$effect_indices == within_effect_index)
                        within_effect_name <- private$within_design$effect_names[within_effect_index+1]
                        between_b_indices <- which(private$between_design$effect_indices == between_effect_index)
                        between_effect_name <- private$between_design$effect_names[between_effect_index+1]
                        covariates_b_indices <- which(private$covariates_design$effect_indices == covariates_effect_index)
                        covariates_effect_name <- private$covariates_design$effect_names[covariates_effect_index+1]

                        for (covariates_index in covariates_b_indices) {
                            for (between_index in between_b_indices) {
                                for (within_index in within_b_indices) {
                                    # within_index <- 5
                                    # between_index <- 1
                                    # covariates_index <- 1

                                    # print("inner loop")
                                    # print(covariates_index)
                                    # print(between_index)
                                    # print(within_index)

                                    # counter <- counter+1
                                    # print(counter)
                                    # print("")

                                    col_name <-
                                        paste0(c(
                                            if (within_index == 1) NULL else names(within_data)[within_index + length(private$id)],
                                            if (between_index == 1) NULL else names(between_data)[between_index + length(private$id)],
                                            if (covariates_index == 1) NULL else names(covariates_data)[covariates_index + length(private$id)]
                                        ), collapse = ":")
                                    col_name <- if (col_name == "") "(Intercept)" else col_name

                                    within_col <- within_data %>%
                                        select(-all_of(private$id)) %>%
                                        `[[`(within_index)

                                    between_col <- between_data %>%
                                        select(-all_of(private$id)) %>%
                                        `[[`(between_index)

                                    covariates_col <- covariates_data %>%
                                        select(-all_of(private$id)) %>%
                                        `[[`(covariates_index)

                                    b_matrix <- bind_cols(
                                        b_matrix,
                                        tibble(.new_col = within_col * between_col * covariates_col)
                                    )
                                    colnames(b_matrix)[ncol(b_matrix)] <- col_name
                                    attr_assign <- c(attr_assign, effect_index)
                                }
                            }
                        }
                    }
                }
            }

            attr_formula <- paste0(
                "(", as.character(within_terms)[2], ")*",
                "(", as.character(between_terms)[2], ")*",
                "(", as.character(covariates_terms)[2], ")"
            ) %>%
                Deriv::Simplify() %>%
                (function(x) paste0("dv ~", x)) %>%
                as.formula()

            terms <- attr_formula

            attr_variables <- str2lang(paste0(
                "list(dv,",
                paste0(attr_variables, collapse = ","),
                ")"
            ))

            class(terms) <- c("terms", "formula")
            attributes(terms) <- list(
                variables = attr_variables,
                factors = attr_factors,
                term.labels = attr_term_labels,
                order = attr_order,
                intercept = 1,
                response = 1,
                .Environment = environment()
            )

            b_matrix <- b_matrix %>%
                select(-all_of(private$id)) %>%
                as.matrix()

            attr(b_matrix, "assign") <- attr_assign
            attr(b_matrix, "contrasts") <- attr_contrasts

            model_frame <- private$.get_model_frame()

            xlevels <- c(
                apply(private$within_design$idata, 2, unique),
                apply(private$between_design$idata, 2, unique, simplify = F),
                private$covariates_description[sapply(private$covariates_description, function(x) !is.numeric(x))]
            )
            names(xlevels) <- c(
                names(private$within_design$idata),
                names(private$between_design$idata),
                names(private$covariates_description)[sapply(private$covariates_description, function(x) !is.numeric(x))]
            )

            contrast_matrix <- lapply(private$hypotheses, function(hypothesis) hypothesis$get_contrasts(self)) %>%
                (function(x) do.call(cbind, x)) %>%
                t()

            par_labels <- c(self$get_par_labels)
            vcov <- lavInspect(private$sem_obj, "vcov")[par_labels,par_labels]
            est <- self$get_estimates() %>%
                filter(label %in% par_labels)
            est <- est[sapply(par_labels, function(label) which(label == est$label)),][["est"]] %>%
                matrix(ncol = 1)
            est <- contrast_matrix %*% est
            rownames(est) <- colnames(b_matrix)

            vcov <- contrast_matrix %*% vcov %*% t(contrast_matrix)
            dimnames(vcov) <- list(
                colnames(b_matrix),
                colnames(b_matrix)
            )

            list(
                model_frame = model_frame,
                xlevels = xlevels,
                model_matrix = b_matrix,
                terms = terms,
                formula = attr_formula,
                vcov = vcov,
                coefficients = est
            )
        }
    ),
    active = list(
        formula = function() {
#
#             within_terms <- terms(private$within_design$idesign)
#             between_terms <- terms(private$between_design$idesign)
#             covariate_terms_names <- private$covariates_design
#             expand.grid(
#                 attr(within_terms, "term.labels"),
#                 attr(between_terms, "term.labels"),
#                 covariate_terms_names
#             )
#
#             betweens <- names(private$betweens)
#             withins <- names(private$withins)
#             terms(private$within_design$idesign)
#             private$covariates_design
#             covariates <- private$covariates
#             as.formula(
#                 paste0(
#                     "dv ~ ",
#                     paste0(c(
#                         paste0(betweens, collapse = "*"),
#                         paste0(withins, collapse = "*"),
#                         paste0(covariates, collapse = "*")
#                     ), collapse = "*")
#                 )
#             )
            private$model_info$formula
        },
        terms = function() {
            private$model_info$terms
        },
        family = function() {
            gaussian()
        },
        fixed_effects_means = function(point_estimates = T) {
            # coefficients <- self$get_estimates()
            # indices <- sapply(c(self$get_par_labels), function(label) which(label == coefficients$label))
            # coefficients <- coefficients[indices,]
            # if (point_estimates) {
            #     coefficients <- coefficients[["est"]]
            #     names(coefficients) <- c(self$get_par_labels)
            # }
            # coefficients
            private$model_info$coefficients
        },
        fixed_effects_vcov = function() {
            vcov <- vcov(self$get_sem_object)
            vcov <- vcov[,c(self$get_par_labels)]
            vcov <- vcov[c(self$get_par_labels),]
            private$model_info$vcov
        },
        model_matrix = function() {
            self$get_model_matrix()
        },
        model_frame = function() {
            private$model_info$model_frame
        },
        model_offset = function() {
            # private$model_info$model_offset
            NULL
        },
        xlevels = function() {
            private$model_info$xlevels
        },
        vcov = function() {
            private$model_info$vcov
        },
        coefficients = function() {
            private$model_info$coefficients
        }
    ),
    public = list(

        #' @export
        get_model_matrix = function(dat = NULL) {
            private$.get_model_info(dat)$model_matrix
        },

        #' @description Semnova class constructor.
        initialize = function() {
            super$initialize()
            invisible(self)
        },

        #' @description Specifies a Semnova object.
        #' @param data tibble. Will be converted to tibble if not already a
        #' tibble.
        #' @param id Character vector. Variables from the data set that
        #' uniquely identify each case.
        #' @param dv Character. Variable from the data set that contains the
        #' dependent variable.
        #' @param indicators Character. Variable from the data set that
        #' contains indicators measuring the latent dependent eta variables.
        #' @param withins Character vector. Variables from the data set that
        #' contain within-subject factors.
        #' @param betweens Character vector. Variables from the data set that
        #' contain between-subject factors.
        #' @param contrasts_arg Named list of characters. Each element specifies
        #' the contrast coding scheme for the within or between subject factors.
        #' Names of the list elements indicate the factor.
        #' @param resid_cov List of character vectors. Each character vector
        #' contains manifest variables from the data set among which residual
        #' covariances should be implemented. Residual covariances can vary.
        #' @param equal_resid_cov List of character vectors. Each character vector
        #' contains manifest variables from the data set among which residual
        #' covariances should be implemented. Residual covariances are
        #' constrained to be equal.
        #' @param compound_symmetry Logical. Indicates whether compound symmetry
        #' should be imposed.
        #' @param sphericity List of character vectors or formula. If list: Each
        #' element contains factor names for which sphericity should be imposed;
        #' If formula: Left-hand side is either \code{TRUE} or \code{FALSE}. If
        #' left-hand side is \code{TRUE}, sphericty is assumed except for effect
        #' names occuring on the right-hand side. If \code{FALSE}, sphericty is
        #' NOT assumed except for effect names occuring on the right-hand side.
        #' @param covariates Named list of character vectors. Each list element
        #' (character vector) represents an covairate variable. The elements
        #' of the character vector are manifest variables (i.e., indicators)
        #' from the data set that measure the corresponding covariate. The list
        #' names correspond to the names of the covariates.
        #' E.g.:
        #' \code{
        #' covariates = list(
        #'     covariate1 = c("covariate11", "covariate12"),
        #'     covariate2 = c("covariate21", "covariate22"),
        #'     covariate3 = c("covariate31", "covariate32")
        #' )}
        #' @param normalize_contrasts Logical. Indicates whether contrasts
        #' should be scaled to length equal to one.
        #' @param ... Parameters passed to the Lgc Class constructor.
        #' @export
        #' @import tidyverse
        #' @importFrom vctrs vec_as_names
        specify = function(
            data,
            id,
            dv,
            indicators = NULL,
            withins = NULL,
            betweens = NULL,
            contrasts_arg = list(),
            resid_cov = list(),
            equal_resid_cov = list(),
            sphericity = list(),
            covariates = NULL,
            normalize_contrasts = FALSE,
            ...
        ) {

            # TODO:
            # - add deprecated warning for "within" argument
            # - add deprecated warning for "between" argument
            # - add deprecated warning for "indicator" argument


            # 0. deprecated ----------------------------------------------------

            dots <- list(...)
            if (!is.null(dots[["within"]])) {
                withins <- within
                warning('Argument "within" is deprecated. Please use "withins" instead.')
            }
            if (!is.null(dots[["between"]])) {
                betweens <- dots$between
                warning('Argument "between" is deprecated. Please use "betweens" instead.')
            }
            if (!is.null(dots[["indicator"]])) {
                indicators <- dots$indicator
                warning('Argument "indicator" is deprecated. Please use "indicators" instead.')
            }

            # specify: 1. sort data (not necessary but looks nice) -------------

            data <- data %>%
                arrange_at(c(
                    id,
                    betweens,
                    withins,
                    indicators
                )) %>%
                mutate_all(function(x) if (is.factor(x)) droplevels(x) else x)

            # specify: 2. parse withins argument -------------------------------

            if (is.null(withins)) {
                withins <- list(.A = "A1")
                data$.A = "A1"
            } else {
                withins_names <- withins
                withins <- lapply(withins, function(x) unique(data[[x]]))
                names(withins) <- withins_names
            }

            # specify: 3. parse indicators argument ----------------------------

            if (is.null(indicators)) {
                data <- data %>%
                    mutate(.indicators = !!dv)

                indicators <- list(.indicators = dv)
            } else {
                indicators_names <- indicators
                indicators <- lapply(indicators, function(x) unique(data[[x]]))
                names(indicators) <- indicators_names
            }

            # specify: 3. parse betweens argument -------------------------------

            group <- private$get_data_name(what = "group")

            if (is.null(betweens)) {
                betweens <- list("1")
                names(betweens) <- group
                data <- data %>%
                    mutate(!!group := "1")
            } else {
                betweens_names <- betweens
                betweens <- lapply(betweens, function(x) unique(data[[x]]))
                names(betweens) <- betweens_names
            }

            private$data_original <- data

            data <- data %>%
                unite(!!group, !!names(betweens))

            # specify: 4. create covariate interactions ------------------------

            if (is.null(covariates)) {
                manifest_covariates <- NULL
                latent_covariates <- NULL
                covariates_description <- NULL
                covariates_mmodel <- NULL
            } else {
                if (is.character(covariates)) {

                    # covariates is a character vector (manifest variables)
                    manifest_covariates <- covariates
                    latent_covariates <- NULL

                } else if (is.list(covariates)) {

                    # covariates is a list (latent variables)
                    manifest_covariates <-
                        unlist(covariates[sapply(covariates, function(x) length(x) == 1L)])
                    latent_covariates <-
                        covariates[sapply(covariates, function(x) length(x) > 1L)]
                    latent_covariates <- if (length(latent_covariates) > 0L) latent_covariates else NULL
                }

                covariates_description <-
                    private$get_covariates_description(
                        data = data,
                        manifest_covariates = manifest_covariates
                    )

                expanded_data <- private$expand_data_covariates(
                    data = data,
                    latent_covariates = latent_covariates,
                    manifest_covariates = manifest_covariates,
                    contrasts_arg = contrasts_arg
                )

                data <- expanded_data$data
                covariates_mmodel <- expanded_data$covariates_mmodel
            }



            # initialize: 10. reshape data -------------------------------------

            selected_vars <- c(
                id, dv, names(indicators), names(withins),
                group, unlist(covariates_mmodel, use.names = FALSE)
            )

            uniquely_identified <- data %>%
                select_at(selected_vars) %>%
                group_by_at(c(
                    id,
                    group,
                    names(indicators),
                    unlist(covariates_mmodel, use.names = FALSE),
                    names(withins)
                )) %>%
                summarize(n = n(), .group = "keep") %>%
                ungroup() %>%
                summarize(n = max(n)) %>%
                `[[`("n")

            if (uniquely_identified != 1L) {
                stop("Cases are not uniquely identified.")
            }

            private$data_original <- private$data_original %>%
                select_at(c(
                    id, dv, names(indicators), names(withins),
                    names(betweens), unlist(covariates_mmodel, use.names = FALSE))
                )

            dat_wide <- data %>%
                select_at(selected_vars) %>%
                pivot_wider(
                    id_cols = all_of(c(
                        id,
                        group,
                        unlist(covariates_mmodel, use.names = FALSE)
                    )),
                    names_from = all_of(c(names(indicators),names(withins))),
                    values_from = all_of(dv),
                    names_prefix = "."
                )

            if (is.character(sphericity)) {
                sphericity <- list(sphericity)
            }

            private$specify_internal(
                withins = withins,
                betweens = betweens,
                manifest_covariates = manifest_covariates,
                covariates_description = covariates_description,
                latent_covariates = latent_covariates,
                indicators = indicators,
                id = id,
                dv = dv,
                contrasts_arg = contrasts_arg,
                resid_cov = resid_cov,
                equal_resid_cov = equal_resid_cov,
                sphericity = sphericity,
                normalize_contrasts = normalize_contrasts,
                data = dat_wide,
                ...
            )

            invisible(self)
        },

        #' @description Specifies a Semnova object.
        #' @example
        #' if (FALSE) {
        #'     semnova_dummy(
        #'          n_within = c(2, 2),
        #'          n_between = c(2),
        #'          n_indicators = 2,
        #'          n_latent_covariates = c(2),
        #'          n_manifest_covariates = c(1, 2),
        #'          contrasts_arg_within = c("contr.poly", "contr.poly"),
        #'          contrasts_arg_between = c("contr.treatment"),
        #'          contrasts_arg_covariates = c("contr.treatment"),
        #'          sphericity = list(c(1), c(1,2)),
        #'          resid_cov = list(c(1,2))
        #'      )
        #' }
        specify_dummy = function(
            n_within = 1,
            n_between = 1,
            n_indicator = 1,
            n_latent_covariate = NULL,
            n_manifest_covariate = NULL,
            contrasts_arg_within = NULL,
            contrasts_arg_between = NULL,
            contrasts_arg_covariate = NULL,
            sphericity = NULL,
            resid_cov = NULL,
            equal_resid_cov = NULL,
            ...
        ) {


            # 1. create withins

            if (is.null(n_within) || (length(n_within) == 1L && n_within[1] == 0)) {
                n_within <- 1
            }

            withins <- lapply(1:length(n_within), function(within_index) {
                n <- n_within[within_index]
                paste0("W", within_index, "_", 1:n)
            })
            names(withins) <- paste0("W", 1:length(n_within))



            if (length(contrasts_arg_within) == 0L) {
                contrasts_arg_within <- NULL
            } else if (length(contrasts_arg_within == 1L)) {
                contrasts_arg_within <- rep_len(contrasts_arg_within, length(withins))
            } else if (length(contrasts_arg_within) != length(withins)) {
                stop("contrasts_arg_within must be length 1 or the same length as n_within.")
            }

            if (!is.null(contrasts_arg_within)) {
                contrasts_arg_within <- as.list(contrasts_arg_within)
                names(contrasts_arg_within) <- names(withins)
            }



            # 2. create betweens

            if (is.null(n_between) || (length(n_between) == 1L && n_between[1] == 0)) {
                n_between <- 1
            }

            betweens <- lapply(1:length(n_between), function(between_index) {
                n <- n_between[between_index]
                paste0("B", between_index, "_", 1:n)
            })
            names(betweens) <- paste0("B", 1:length(n_between))


            if (length(contrasts_arg_between) == 0L) {
                contrasts_arg_between <- NULL
            } else if (length(contrasts_arg_between == 1L)) {
                contrasts_arg_between <- rep_len(contrasts_arg_between, length(betweens))
            } else if (length(contrasts_arg_between) != length(betweens)) {
                stop("contrasts_arg_between must be length 1 or the same length as n_between.")
            }

            if (!is.null(contrasts_arg_between)) {
                contrasts_arg_between <- as.list(contrasts_arg_between)
                names(contrasts_arg_between) <- names(betweens)
            }

            # 3. create manifest covariates

            n_manifest_covariate <- n_manifest_covariate[n_manifest_covariate != 0L]

            if (is.null(n_manifest_covariate)) {
                manifest_covariates <- NULL
                covariates_description <- NULL
            } else if (length(n_manifest_covariate) == 0L) {
                manifest_covariates <- NULL
                covariates_description <- NULL
            } else {
                manifest_covariates <- paste0("covariate", 1:length(n_manifest_covariate))
                covariates_description <-
                    lapply(1:length(n_manifest_covariate), function(covariate_index) {
                        if (n_manifest_covariate[covariate_index] == 1L) {
                            numeric()
                        } else {
                            paste0(
                                "covariate",
                                covariate_index,
                                "_",
                                1:n_manifest_covariate[covariate_index]
                            )
                        }
                    })
                names(covariates_description) <- manifest_covariates

                if (length(contrasts_arg_covariate) == 0L) {
                    contrasts_arg_covariate <- NULL
                } else if (length(contrasts_arg_covariate == 1L)) {
                    contrasts_arg_covariate <- rep_len(contrasts_arg_covariate, length(manifest_covariates))
                } else if (length(contrasts_arg_covariate) != length(manifest_covariates)) {
                    stop("contrasts_arg_covariate must be length 1 or the same length as n_covariates.")
                }

                if (!is.null(contrasts_arg_covariate)) {
                    contrasts_arg_covariate <- contrasts_arg_covariate[n_manifest_covariate != 1]
                    contrasts_arg_covariate <- as.list(contrasts_arg_covariate)
                    names(contrasts_arg_covariate) <- manifest_covariates[n_manifest_covariate != 1]
                }
            }

            contrasts_arg <- c(
                contrasts_arg_within,
                contrasts_arg_between,
                contrasts_arg_covariate
            )

            # 4. create latent covariates

            n_latent_covariate <- n_latent_covariate[n_latent_covariate != 0L]

            if (is.null(n_latent_covariate)) {
                latent_covariates <- NULL
            } else if (length(n_latent_covariate) == 0L) {
                latent_covariates <- NULL
            } else if (any(n_latent_covariate == 1L)) {
                stop("Latent covariates need at least two indicators.")
            } else {
                latent_covariates <- lapply(1:length(n_latent_covariate), function(covariate_index) {
                    paste0("covariate", covariate_index+length(n_manifest_covariate), "_", 1:n_latent_covariate[covariate_index])
                })
                names(latent_covariates) <- paste0("covariate", (1:length(n_latent_covariate))+length(n_manifest_covariate))
            }

            # 5. create the small things

            id <- "id"
            dv <- "dv"

            if (is.null(n_indicator) || n_indicator == 1L) {
                n_indicator <- 1
            }

            indicators <- list(indicators = paste0("Y", 1:n_indicator))

            # 6. sphericity

            if (is.null(sphericity)) {
                sphericity <- list()
            }

            sphericity <- lapply(sphericity, function(x) {
                paste0("W", x)
            })

            # 7. resid_cov

            if (is.null(resid_cov)) {
                resid_cov <- list()
            }

            resid_cov <- lapply(resid_cov, function(x) {
                paste0("Y", 1:n_indicator)[x]
            })


            if (is.null(equal_resid_cov)) {
                equal_resid_cov <- list()
            }

            equal_resid_cov <- lapply(equal_resid_cov, function(x) {
                paste0("Y", 1:n_indicator)[x]
            })

            # 8. estimate model

            private$specify_internal(
                withins = withins,
                betweens = betweens,
                manifest_covariates = manifest_covariates,
                covariates_description = covariates_description,
                latent_covariates = latent_covariates,
                indicators = indicators,
                id = id,
                dv = dv,
                contrasts_arg = contrasts_arg,
                resid_cov = resid_cov,
                equal_resid_cov = equal_resid_cov,
                sphericity = sphericity,
                dummy = TRUE,
                ...
            )

            invisible(self)
        }

        # #' @description Returns the point estimates of the model.
        # #' @param ... Additional arguments.
        # #' @importFrom lavaan parTable
        # #' @importFrom stringr str_detect str_extract
        # get_estimates = function(
        #     ...
        # ) {
        #
        #     estimates <- super$get_estimates(...)
        #
        #     idata <- private$parse_between_design(
        #         private$betweens,
        #         list(),
        #         FALSE
        #     )$idata
        #
        #     bind_cols(
        #         estimates,
        #         idata[estimates$group_index,,drop = F] %>% as_tibble()
        #     )
        #
        # }

    )
)

#' @rdname semnova
#' @export
semnova <- function (...) {
    Semnova$new()$specify(...)
}

#' @rdname semnova
#' @export
semnova_dummy <- function (...) {
    Semnova$new()$specify_dummy(...)
}

#' @export
Effect <- function(focal.predictors, mod, ...){
    UseMethod("Effect", mod)
}

#' @rdname semnova
#' @export
Effect.Semnova <- function (focal.predictors, mod, ...) {
    SemnovaEffect$new(mod)$effect(focal.predictors, ...)
}

#' @rdname semnova
#' @export
plot.Semnova <- function (mod, focal.predictors, ...) {
    plot(Effect(focal.predictors, mod, ...), ...)
}
