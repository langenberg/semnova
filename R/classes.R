#' @keywords internal
setClass("par_table",
         representation(
             id    = "numeric",
             lhs   = "character",
             op    = "character",
             rhs   = "character",
             free  = "numeric",
             value = "character",
             group = "numeric",
             na    = "numeric"
         )
)

#' LGC Class.
#' @export
setClass("lgc",
         representation(
             dvs                = "character",
             lvs                = "character",
             indicators         = "character",
             groups             = "character",
             lv_labels          = "character",
             mmodel             = "par_table",
             intercepts         = "par_table",
             variances          = "par_table",
             struc_coeff        = "par_table",
             regressions        = "par_table",
             constraints        = "par_table",
             C_matrix           = "matrix",    # contrast matrix, dimension
             sem_obj            = "ANY",    # sem object
             append             = "ANY",
             model_string       = "character", # sem model string
             hypotheses         = "ANY",
             opts               = "list",
             N                  = "numeric"
         )
)

#' Comparing the fit of LGC objects.
#' @export
#' @param object \code{lgc} object. An \code{lgc} object to be compared against other \code{lgc} objects.
#' @param ... \code{lgc} object. More \code{lgc} objects to be compared.
setMethod('anova', signature(object = "lgc"),
          function(object, ...) {
              # Mostly copied from the lavaan package.
              # Thanks, Yves!!

              object <- object@sem_obj

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

              modp <- sapply(dots, is, "lgc")
              mods <- c(list(object), dots[modp])
              NAMES <- sapply(as.list(mcall)[c(FALSE, TRUE, modp)], deparse)
              dots <- lapply(dots, function(x) if (is(x, "lgc")) x@sem_obj else x)

              ans <- do.call("lavTestLRT", c(list(object = object,
                                                  SB.classic = SB.classic, SB.H0 = SB.H0,
                                                  model.names = NAMES), dots))
              ans
          })
