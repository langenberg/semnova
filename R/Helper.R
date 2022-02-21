
#' Helper class
#' @description Some description
#' @noRd
#' @importFrom R6 R6Class
Helper <- R6::R6Class(
    "Helper",
    private = list(
        #' @description Helper function.
        #' @keywords internal
        list_to_str = function(
            l,
            sep = ", ",
            before_element = "",
            after_element = "",
            before_name = "",
            ...
        ) {

            if (is.null(l)) {
                return("NULL")
            } else if (length(l) == 0L) {
                return("list()")
            }


            l_names <- names(l)
            names(l) <- NULL
            l <- sapply(l, self$to_str)
            l <- paste0(before_element, l, after_element)

            if (!is.null(l_names) && all(l_names == "")) {
                l_names <- NULL
            }

            if (!is.null(l_names)) {
                l <- sapply(1:length(l), function(index) {
                    if (l_names[index] == "") {
                        l[index]
                    } else {
                        paste0(before_name, l_names[index], " = ", l[index])
                    }
                })
            }


            l <- paste0("list(", paste0(l, collapse = ", "), ")")

            l
        },

        #' @description Helper function.
        #' @keywords internal
        vector_to_str = function(vec, default = character, ...) {
            empty <- function() {
                case_when(
                    inherits(default(), "character") ~ "character()",
                    inherits(default(), "numeric") ~ "numeric()",
                    inherits(default(), "logical") ~ "logical()"
                )
            }

            if (is.null(vec) || length(vec) == 0L) {
                "NULL"
            } else if (length(vec) == 0L) {
                empty()
            } else if (is.numeric(vec) || is.logical(vec)) {
                private$vector_to_str_numeric(vec)
            } else if (is.character(vec)) {
                private$vector_to_str_character(vec)
            } else {
                stop("Unknown vector class.")
            }
        },

        #' @description Helper function.
        #' @keywords internal
        vector_to_str_character = function(vec, ...) {

            vec <- sapply(vec, function(x) paste0('"', x, '"'), USE.NAMES = FALSE)

            private$vector_to_str_numeric(vec)
        },

        #' @description Helper function.
        #' @keywords internal
        vector_to_str_numeric = function(vec, ...) {

            vec_names <- names(vec)

            if (!is.null(vec_names) && all(vec_names == "")) {
                vec_names <- NULL
            }

            if (!is.null(vec_names)) {
                vec <- sapply(1:length(vec), function(index) {
                    if (vec_names[index] == "") {
                        vec[index]
                    } else {
                        paste0(vec_names[index], " = ", vec[index])
                    }
                })
            }

            if (length(vec) > 1L || !is.null(vec_names)) {
                vec <- paste0("c(", paste0(vec, collapse = ", "), ")")
            }

            vec
        },

        #' @description Helper function.
        #' @keywords internal
        matrix_to_str = function(mat, prefix = "", ...) {
            paste0(
                "matrix(c(\n",
                prefix, "    ",
                apply(mat, 1, paste0, collapse = ", ") %>%
                    paste0(collapse = paste0(",\n", prefix, "    ")),
                "\n",
                prefix, "),", " nrow = ", nrow(mat), ", byrow = T)"
            )
        },

        #' @description Helper function.
        #' @keywords internal
        hypothesis_to_str = function(hypothesis, ...) {
            description <- hypothesis$get_description
            M <- hypothesis$get_M
            L <- hypothesis$get_L
            P <- hypothesis$get_P

            paste0(
                "Hypothesis$new(\n",
                if (description == "") "" else paste("description = ", self$to_str(description), ",\n"),
                paste0("M = ", self$to_str(M), ",\n"),
                paste0("L = ", self$to_str(L), ",\n"),
                paste0("P = ", self$to_str(P)),
                "\n)"
            )
        }
    ),

    active = list(

        #' @field expose Read only. Returns the private structure of the object. For debugging purposes only.
        expose = function() {
            private
        }
    ),

    public = list(
        #' @description Helper function.
        #' @keywords internal
        #' @importFrom styler style_text
        args_to_str = function(
            args,
            prefix = "",
            print_names = TRUE,
            sep = ",\n",
            ...
        ) {

            if (is.null(args) || length(args) == 0L) {
                return("")
            }

            results <- lapply(1:length(args), function(arg_index) {
                arg <- args[[arg_index]]
                arg_name <- names(args)[arg_index]
                if (is.null(arg_name) || length(arg_name) == 0L || arg_name == "" || !print_names) {
                    arg_name <- ""
                } else {
                    arg_name <- paste0(arg_name, " = ")
                }
                paste0(prefix, arg_name, self$to_str(arg))
            })

            results <- results[sapply(results, function(x) x != "")]

            results %>% paste0(collapse = sep)
        },

        to_str = function(x, ...) {
            if (inherits(x, "Hypothesis")) {
                private$hypothesis_to_str(x, ...) %>% styler::style_text() %>% paste0(collapse = "\n")
            } else if (inherits(x, "list")) {
                private$list_to_str(x, ...) %>% styler::style_text() %>% paste0(collapse = "\n")
            } else if (inherits(x, "matrix")) {
                private$matrix_to_str(x, ...) %>% styler::style_text() %>% paste0(collapse = "\n")
            } else if (inherits(x, "numeric") || inherits(x, "character") || inherits(x, "logical")) {
                private$vector_to_str(x, ...) %>% styler::style_text() %>% paste0(collapse = "\n")
            } else {
                "unknown class"
            }
        }
    )
)
