
#' SemnovaEffect class
#' @description Some description
#' @export
#' @rdname SemnovaEffect
#' @importFrom R6 R6Class
SemnovaEffect <- R6::R6Class(
    "SemnovaEffect",
    private = list(
        .mod = NULL,
        .v = NULL,
        .d = NULL,
        .u = NULL
    ),
    active = list(
        #' @field expose Read only. Returns the private structure of the object. For debugging purposes only.
        expose = function() {
            private
        },
        formula = function() {
            new_formula <- private$.mod$formula
            class(new_formula) <- c("SemnovaFormula", class(new_formula))
            attr(new_formula, "effect") <- self
            new_formula
        },
        terms = function() {
            private$.mod$terms
        },
        family = function() {
            private$.mod$family
        },
        model = function() {
            private$.mod
        },
        model.frame = function() {
            private$.mod$model_frame
        },
        model.offset = function() {
            private$.mod$model_offset
        },
        model.matrix = function() {
            private$.mod$model_matrix
        },
        call = function() {
            self$getCall
        },
        getCall = function() {
            list(
                data = self$model.frame %>% mutate(dv = 0) %>% na.omit(),
                subset = NULL,
                na.action = NULL
            )
        },
        xlevels = function() {
            private$.mod$xlevels
        },
        vcov = function() {
            private$.mod$vcov
        },
        coefficients = function() {
            private$.mod$coefficients
        },
        qr = function() {
            self$model.matrix %>%
                na.omit() %>%
                qr()
        },
        d = function() {
            private$.d
        },
        u = function() {
            private$.u
        },
        v = function() {
            private$.v
        }
    ),
    public = list(
        get_model_matrix = function(data = NULL, ...) {
            private$.mod$get_model_matrix(data)
        },
        initialize = function(mod) {
            private$.mod <- mod
            mysvd <- self$model.matrix %>%
                na.omit() %>%
                svd()
            private$.d = mysvd$d
            private$.u = mysvd$u
            private$.v = mysvd$v
        },
        #' @import effects
        effect = function(focal.predictors, ...) {
            effects:::Effect.lm(focal.predictors, self, vcov. = self$vcov, ..., sources = self$effSources())
        },
        effSources = function() {
            fit <- private$.mod
            betweens <- names(fit$expose$betweens)
            withins <- names(fit$expose$withins)
            covariates <- fit$expose$covariates

            myformula <- as.formula(
                paste0(
                    "dv ~ ",
                    paste0(c(
                        paste0(betweens, collapse = "*"),
                        paste0(withins, collapse = "*"),
                        paste0(covariates, collapse = "*")
                    ), collapse = "*")
                )
            )

            vcov <- vcov(fit$get_sem_object)
            vcov <- vcov[,c(fit$get_par_labels)]
            vcov <- vcov[c(fit$get_par_labels),]

            coefficients <- fit$get_estimates()
            indices <- sapply(c(fit$get_par_labels), function(label) which(label == coefficients$label))
            coefficients <- coefficients[indices,][["est"]]
            names(coefficients) <- c(fit$get_par_labels)

            list(
                type = "semnova",
                call = match.call(),
                formula = myformula,
                coefficients = coefficients,
                vcov = vcov,
                # data = NULL,
                # contrasts = NULL,
                # subset = NULL,
                # offset = NULL,
                family = gaussian()
            )
        }
    )
)

#' @export
effSources <- function(mod){
    UseMethod("effSources", mod)
}

#' @rdname SemnovaEffect
#' @export
effSources.SemnovaEffect <- function(mod) {
    mod$effSources()
}

#' @rdname SemnovaEffect
#' @export
formula.SemnovaEffect <- function(x, ...) {
    x$formula
}

#' @rdname SemnovaEffect
#' @export
family.SemnovaEffect <- function(object, ...) {
    object$family
}

#' @rdname SemnovaEffect
#' @export
model.frame.SemnovaEffect <- function(object, ...) {
    object$model.frame
}

#' @rdname SemnovaEffect
#' @export
model.offset.SemnovaEffect <- function(object, ...) {
    object$model.offset
}

#' @rdname SemnovaEffect
#' @export
model.matrix.SemnovaEffect <- function(object, ...) {
    object$get_model_matrix(...)
}

#' @rdname SemnovaEffect
#' @export
expand.model.frame.SemnovaEffect <- function (model, extras, envir = environment(formula(model)), na.expand = FALSE) {
    object$model.frame
}

#' @rdname SemnovaEffect
#' @export
#' @import tidyverse
getCall.SemnovaEffect <- function (x, ...) {
    x$getCall
}

#' @rdname SemnovaEffect
#' @export
vcov.SemnovaEffect <- function (object, complete = TRUE, ...) {
    object$vcov
}

#' @rdname SemnovaEffect
#' @export
coefficients.SemnovaEffect <- function (object, ...) {
    object$coefficients
}

#' @rdname SemnovaEffect
#' @export
coef.SemnovaEffect <- function (object, ...) {
    object$coefficients
}

#' @rdname SemnovaEffect
#' @export
nonest.basis <- function (x, ...) {
    UseMethod("nonest.basis")
}

#' @rdname SemnovaEffect
#' @export
#' @import estimability
nonest.basis.SemnovaEffect <- function (x, ...) {
    estimability:::nonest.basis.lm(x)
}

#' @rdname SemnovaEffect
#' @export
qr.SemnovaEffect <- function (x, ...) {
    x$qr
}

#' @rdname SemnovaEffect
#' @export
model.matrix.SemnovaFormula <- function(object, ...) {
    # object$get_model_matrix(...)
    attr(object, "effect")$get_model_matrix(...)
}

#' @rdname SemnovaEffect
#' @export
terms.SemnovaFormula <- function(x, ...) {
    attr(x, "effect")$terms
}

#' @rdname SemnovaEffect
#' @export
#' @method `[` SemnovaFormula
`[.SemnovaFormula` <- function (x, i) {
    class(x) <- "formula"
    new_formula <- x[i]
    class(new_formula) <- c("SemnovaFormula", class(new_formula))
    attr(new_formula, "effect") <- attr(x, "effect")
    new_formula
}


