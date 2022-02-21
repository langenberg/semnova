
#' Power class
#' @description Some description
#' @export
#' @importFrom R6 R6Class
Power <- R6::R6Class(
    "Power",
    private = list(

        mod = NULL,
        results = NULL,

        power_template_lgc =
'
data_syntax <- "
##data_syntax##
"

hypotheses = list(
##hypothesis##
)

power <- power_analysis_lgc(
##args##,
    hypotheses = hypotheses,
    data_syntax = data_syntax,
    sample_size = ##sample_size##,
    replications = 500
)

# hypothesis tests
summary(power)

# parameters
print(power)

# path diagram
plot(power)
',
    power_template_semnova =
'
data_syntax <- "
##data_syntax##
"

power <- power_analysis_semnova(
##args##,
    data_syntax = data_syntax,
    sample_size = ##sample_size##,
    replications = 500
)

# hypothesis tests
summary(power)

# parameters
print(power)

# path diagram
plot(power)
'
    ),
    active = list(

        #' @field expose Read only. Returns the private structure of the object. For debugging purposes only.
        expose = function() {
            private
        }

    ),
    public = list(
        initialize = function() {

        },

        #' @description Returns a list of detailed results.
        get_results = function() {
            private$results
        },

        #' @description Creates a power script for the Lgc class.
        #' @importFrom stringr str_replace
        #' @importFrom styler style_text
        power_script_lgc = function(
            ...,
            out_file = NULL,
            print = FALSE
        ) {

            dots <- list(...)

            mod <- Lgc$new()
            mod <- do.call(mod$specify_dummy, dots)

            args_str <- helper$args_to_str(dots, prefix = "    ")

            data_syntax <- mod$get_lav_syntax(
                fixed_covariates = TRUE,
                remove_unfree = FALSE,
                remove_na = TRUE,
                remove_constraints = TRUE,
                remove_definitions = TRUE,
                break_rhs = FALSE
            )

            hypothesis <- mod$get_hypotheses[[1]] %>% helper$to_str()

            sample_size <- rep_len(50, length(mod$get_group_labels)) %>%
                helper$to_str() %>%
                paste0()

            new_script <- private$power_template_lgc %>%
                str_replace("##data_syntax##", data_syntax) %>%
                str_replace("##hypothesis##", hypothesis) %>%
                str_replace("##args##", args_str) %>%
                str_replace("##sample_size##", sample_size) %>%
                style_text() %>%
                paste0(collapse = "\n")

            if (print) {
                cat(new_script)
            }

            if (!is.null(out_file)) {
                writeLines(new_script, out_file)
            }

            invisible(new_script)
        },


        #' @description Creates a power script for the Semnova class.
        #' @importFrom stringr str_replace
        #' @importFrom styler style_text
        #' @example
        #'
        #' if (FALSE) {
        #'     power_script_semnova(
        #'          n_within = c(2, 2),
        #'          n_between = c(2),
        #'          n_indicators = 2,
        #'          n_latent_covariates = c(2),
        #'          n_manifest_covariates = c(1, 2),
        #'          contrasts_arg_within = c("contr.poly", "contr.poly"),
        #'          contrasts_arg_between = c("contr.treatment"),
        #'          contrasts_arg_covariates = c("contr.treatment"),
        #'          sphericity = list(c(1), c(1,2)),
        #'          resid_cov = list(c(1,2)),
        #'          out_file = "power_script.R",
        #'          print = TRUE
        #'      )
        #' }
        power_script_semnova = function(
            ...,
            out_file = NULL,
            print = FALSE
        ) {

            dots <- list(...)

            mod <- Semnova$new()
            mod <- do.call(mod$specify_dummy, dots)

            args_str <- helper$args_to_str(dots, prefix = "    ")

            data_syntax <- mod$get_lav_syntax(
                fixed_covariates = TRUE,
                remove_unfree = FALSE,
                remove_na = TRUE,
                remove_constraints = TRUE,
                remove_definitions = TRUE,
                break_rhs = FALSE
            )

            sample_size <- rep_len(50, length(mod$get_group_labels)) %>%
                helper$to_str() %>%
                paste0()


            # 10. create script

            new_script <- private$power_template_semnova %>%
                str_replace("##data_syntax##", data_syntax) %>%
                str_replace("##args##", args_str) %>%
                str_replace("##sample_size##", sample_size) %>%
                style_text() %>%
                paste0(collapse = "\n")

            if (print) {
                cat(new_script)
            }

            if (!is.null(out_file)) {
                writeLines(new_script, out_file)
            }

            invisible(new_script)
        },

        power_analysis_lgc = function(..., data_syntax, sample_size, replications) {
            private$mod <- Lgc$new()$specify_dummy(...)

            private$results <- private$mod$power_analysis(
                data_syntax,
                sample_size,
                replications
            )

            invisible(self)
        },

        power_analysis_semnova = function(..., data_syntax, sample_size, replications) {
            private$mod <- Semnova$new()$specify_dummy(...)

            private$results <- private$mod$power_analysis(
                data_syntax,
                sample_size,
                replications
            )

            invisible(self)
        },

        #' @description Prints the power for each of the model parameters.
        print = function(...) {

            if (is.null(private$results)) {
                return(invisible(self))
            }

            private$results$estimates_aggregated %>%
                select(-label) %>%
                group_by(group) %>%
                group_walk(function(tbl, desc) {
                    # cat("Group: ")
                    # cat(desc$group)
                    # cat("\n")
                    kableExtra::kable(
                        tbl,
                        format = "simple",
                        caption = if (length(private$mod$get_group_labels) == 1L) NULL else desc$group
                    ) %>%
                        print()
                    invisible()
                })

            invisible(self)
        },

        #' @description Prints the power for the hypotheses.
        summary = function(...) {
            if (is.null(private$results)) {
                return(invisible(self))
            }

            private$results$tests_aggregated %>%
                kableExtra::kable(format = "simple", caption = "Hypotheses") %>%
                print()

            invisible(self)
        },

        #' @description Plots the Lgc dummy object.
        plot = function(...) {
            if (!is.null(private$mod)) {
                private$mod$plot(...)
            }
        }
    )
)

#' Wrapper method for Power$get_power_lgc_script()
#' @export
#' @param ... Additional arugments passed to Power$get_power_lgc_script().
power_script_lgc <- function(...) {
    Power$new()$power_script_lgc(...)
}

#' Wrapper method for Power$get_power_semnova_script()
#' @export
#' @param ... Additional arugments passed to Power$get_power_semnova_script().
power_script_semnova <- function(...) {
    Power$new()$power_script_semnova(...)
}

#' Wrapper method for Power$get_power_semnova_script()
#' @export
#' @param ... Additional arugments passed to Power$get_power_semnova_script().
power_analysis_lgc <- function(...) {
    Power$new()$power_analysis_lgc(...)
}

#' Wrapper method for Power$get_power_semnova_script()
#' @export
#' @param ... Additional arugments passed to Power$get_power_semnova_script().
power_analysis_semnova <- function(...) {
    Power$new()$power_analysis_semnova(...)
}

#' Wrapper method for power$print()
#' @export
#' @method print Lgc
#' @param x Object of class Power.
#' @param ... Arguments passed down to power$print().
print.Power <- function(x, ...) {
    x$print(...)
}

#' Wrapper method for power$summary()
#' @export
#' @method summary Power
#' @param x Object of class Power.
#' @param ... Arguments passed down to power$summary().
summary.Power <- function(x, ...) {
    x$summary(...)
}

#' Wrapper method for power$plot()
#' @export
#' @method plot Power
#' @param x Object of class Power.
#' @param ... Arguments passed down to power$plot().
plot.Power <- function(x, ...) {
    x$plot(...)
}

