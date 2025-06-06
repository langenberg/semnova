
#' Helper class
#' @description Some description
#' @noRd
#' @importFrom R6 R6Class
Dag <- R6::R6Class(
    "Dag",
    private = list(
        .mod = NULL,

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

            group_labels <- private$.mod$get_group_labels
            n_groups <- length(groups)

            # some variables to calculate the positions from

            etas_mmodel <- private$.mod$get_etas_mmodel()
            n_etas <- length(etas_mmodel)
            n_ind_etas <- first(sapply(etas_mmodel, length))
            n_ind_etas_total <- sum(sapply(etas_mmodel, length))

            covariates_mmodel <- private$.mod$get_covariates_mmodel()
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
            }) |> bind_rows()


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
                }) |> bind_rows() |>
                    mutate(is_left = FALSE)
            } else {
                covariates <- tibble()
            }

            coords <- bind_rows(
                covariates,
                etas_pi
            ) |>
                mutate(from_id = 1:n())



            # join coordinates with parameter table

            pt <- private$.mod$get_estimates() |>
                group_by(type) |>
                group_modify(function(tbl, desc) {
                    if (desc$type != "regression") {
                        tbl
                    } else {
                        tbl |>
                            mutate(lhs_tmp = lhs,
                                   lhs = rhs,
                                   rhs = lhs_tmp) |>
                            select(-lhs_tmp)
                    }
                }) |>
                ungroup()

            pt <- left_join(
                pt,
                coords,
                by = c("lhs")
            ) |>
                mutate(to_id = sapply(rhs, function(x) first(from_id[lhs == x])))


            # add residuals

            n_resid_left <- pt |>
                filter(
                    !private$.mod$is_latent(lhs),
                    private$.mod$is_indicator(lhs),
                    private$.mod$belongs_to(lhs, "eta"),
                    type == "resid_var",
                    group_index == 1
                ) |> nrow()
            left_id <- max(pt$from_id, na.rm = T) + 1
            right_id <- left_id + n_resid_left

            pt <- pt |>
                group_by(
                    is_resid_var = type == "resid_var" & private$.mod$is_indicator(lhs),
                    group,
                    left = private$.mod$belongs_to(lhs, "eta")
                ) |>
                group_modify(function(tbl, desc) {
                    if (!desc$is_resid_var) {
                        tbl
                    } else if (desc$left) {
                        left_x <- 0
                        bind_rows(
                            tbl |>
                                mutate(
                                    lhs = "epsilon",
                                    from_id = seq(left_id,left_id + nrow(tbl) - 1),
                                    to_id = NA,
                                    x = left_x,
                                    is_node = TRUE,
                                    variable_type = "latent"
                                ),
                            tbl |>
                                mutate(
                                    rhs = lhs,
                                    lhs = "epsilon",
                                    to_id = from_id,
                                    label = NA,
                                    est = NA,
                                    se = NA,
                                    from_id = seq(left_id,left_id + nrow(tbl) - 1)
                                ),
                            tbl |>
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
                            tbl |>
                                mutate(
                                    lhs = "epsilon",
                                    from_id = seq(right_id,right_id + nrow(tbl) - 1),
                                    to_id = NA,
                                    x = right_x,
                                    is_node = TRUE,
                                    variable_type = "latent"
                                ),
                            tbl |>
                                mutate(
                                    rhs = lhs,
                                    lhs = "epsilon",
                                    to_id = from_id,
                                    label = NA,
                                    est = NA,
                                    se = NA,
                                    from_id = seq(right_id,right_id + nrow(tbl) - 1),
                                ),
                            tbl |>
                                mutate(
                                    rhs = lhs,
                                    lhs = "epsilon",
                                    to_id = seq(right_id,right_id + nrow(tbl) - 1),
                                    from_id = seq(right_id,right_id + nrow(tbl) - 1),
                                )
                        )
                    }
                }) |>
                ungroup() |>
                select(-left, -is_resid_var)


            # add intercept triangle

            max_y <- max(pt$y, na.rm = T)
            left_id <- max(pt$from_id, na.rm = T) + 1
            if (n_ind_etas == 1L) {
                right_id <- left_id
            } else {
                right_id <- left_id + 1
            }

            pt <- pt |>
                group_by(op, group, left = lhs %in% unlist(etas_mmodel)) |>
                group_modify(function(tbl, desc) {
                    if (desc$op != ~ 1) {
                        tbl
                    } else if (desc$left) {
                        if (n_ind_etas == 1L) {
                            tbl |> mutate(is_node = TRUE, type = "node")
                        } else {
                            if (n_ind_etas == 1L) {
                                left_x <- 1
                            } else {
                                left_x <- -1
                            }
                            bind_rows(
                                tbl |> mutate(is_node = TRUE, type = "node"),
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
                                tbl |>
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
                            tbl |> mutate(is_node = TRUE, type = "node"),
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
                            tbl |>
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
                }) |>
                ungroup() |>
                select(-left)


            ## some labeling stuff

            pt <- pt |>
                filter(!(rhs %in% names(private$.mod$get_etas_mmodel())) | type != "intercept") |>
                mutate(
                    name = private$.mod$get_labels(lhs),
                    label = private$.mod$get_labels(label)
                ) |>
                mutate(
                    is_node = ifelse(is.na(is_node), FALSE, TRUE),
                    est = round(est, !!digits),
                    est = case_when(
                        is.na(est) ~ "",
                        !free ~ paste0("ring(", est, ")"),
                        TRUE ~ as.character(est)
                    )
                ) |>
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
                ) |>
                mutate(y = -y + max_y) |>
                filter(
                    free | !(type %in% c("variance", "resid_cov", "resid_var", "covariance"))
                )

            pt
        }
    ),
    active = list(
        expose = function() {
            private
        }
    ),
    public = list(
        #' @description Dag class constructor.
        #' @export
        initialize = function(lgc) {
            private$.mod <- lgc
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

            pt <- private$get_graph_table() |>
                filter(group == first(private$.mod$get_group_labels) | !only_first_group) |>
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
                    is_node & private$.mod$belongs_to(lhs, "pi") ~
                        # ifelse(!is.null(pi.x), pi.x, x),
                        sapply(x, function(x) if(!is.null(pi.x)) pi.x else x),
                    is_node & private$.mod$belongs_to(lhs, "eta") & !private$.mod$is_indicator(lhs) ~
                        # ifelse(!is.null(eta.x), eta.x, x),
                        sapply(x, function(x) if(!is.null(eta.x)) eta.x else x),
                    is_node & private$.mod$belongs_to(lhs, "eta") & private$.mod$is_indicator(lhs) ~
                        # ifelse(!is.null(eta_indicator.x), eta_indicator.x, x),
                        sapply(x, function(x) if(!is.null(eta_indicator.x)) eta_indicator.x else x),
                    is_node & private$.mod$belongs_to(lhs, "covariate") & !private$.mod$is_indicator(lhs) ~
                        # ifelse(!is.null(covariate.x), covariate.x, x),
                        sapply(x, function(x) if(!is.null(covariate.x)) covariate.x else x),
                    is_node & private$.mod$belongs_to(lhs, "covariate") & private$.mod$is_indicator(lhs) ~
                        # ifelse(!is.null(covariate_indicator.x), covariate_indicator.x, x),
                        sapply(x, function(x) if(!is.null(covariate_indicator.x)) covariate_indicator.x else x),
                    TRUE ~ x
                )) |>
                mutate(
                    y = (y-min(y, na.rm = T)) * node_distance_scale.y + min(y, na.rm = T),
                    x = (x-min(x, na.rm = T)) * node_distance_scale.x + min(x, na.rm = T)
                )

            coord <- pt |>
                ungroup() |>
                filter(is_node) |>
                mutate(myfilter = type %in% !!what | type == "node") |>
                # filter(variable_type != "intercept" | "intercept" %in% what) |>
                select(id = from_id, x, y, group, type, lhs, rhs, is_node, myfilter) |>
                arrange(group, id)

            nodes <- pt |>
                filter(is_node) |>
                mutate(myfilter = type %in% !!what | type == "node") |>
                # filter(variable_type != "intercept" | "intercept" %in% what) |>
                select(from_id, name, group, type, variable_type, myfilter, is_node) |>
                arrange(group, from_id)

            edges <- pt |>
                ungroup() |>
                filter(is.na(is_node) | !is_node) |>
                select(from = from_id, to = to_id, label = label, group, type, is_left)

            ylimits <- coord |>
                ungroup() |>
                filter(myfilter) |>
                summarize(
                    min = min(y, na.rm = T) - 0.5 * node_distance_scale.y,
                    max = max(y, na.rm = T) + 0.5 * node_distance_scale.y
                ) |> as.list() |> unlist()

            xlimits <- coord |>
                ungroup() |>
                filter(myfilter) |>
                summarize(
                    min = min(x, na.rm = T) - 0.5 * node_distance_scale.x,
                    max = max(x, na.rm = T) + 0.5 * node_distance_scale.x
                ) |> as.list() |> unlist()

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
                    if (private$.mod$has_within()) {
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
                    if (private$.mod$has_latent_dv()) {
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
                    if (private$.mod$has_latent_covariate()) {
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
                    if (private$.mod$has_latent_dv() && private$.mod$has_resid_cov()) {
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
                    if (private$.mod$has_covariate()) {
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

            if (length(private$.mod$get_group_labels) > 1L && !only_first_group) {
                myplot <- myplot +
                    private$.mod$get_facet_grid()
            }

            myplot
        }
    )
)

#' Wrapper method for Dag$new()
#' @export
#' @param lgc Object of class Lgc.
#' @param ... Arguments passed down to dag$plot().
dag <- function(lgc, ...) {
    Dag$new(lgc)$plot(...)
}
