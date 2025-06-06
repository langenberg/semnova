

#' ContrastPlot class
#' @description Some description
#' @export
#' @rdname ContrastPlot
#' @importFrom R6 R6Class
ContrastPlot <- R6::R6Class(
    "ContrastPlot",
    private = list(

    ),
    active = list(

    ),
    public = list(
        initialize = function() {

        },
        #' @import ggplot2
        plot = function(lgc, hypothesis, geom = c("line", "point"), interval = c("none", "sd", "ci")) {
            geom <- match.arg(geom, choices = c("line", "point", "bar"), several.ok = T)
            interval <- match.arg(interval)
            # lgc <- fit
            # hypothesis <- last(fit$get_hypotheses)
            L <- hypothesis$get_L
            M <- hypothesis$get_M
            G <- hypothesis$get_G
            B_array <- lgc$B_array_est
            B_vec <- lgc$B_vec_est
            B_vec_vcov <- lgc$B_vec_vcov
            B_var_vec_est <- lgc$B_var_vec_est

            R <- semnova:::helper$construct_R_matrix(
                B_array = B_array,
                L = L,
                M = M,
                G = G
            )$R

            # est
            est <- as.vector(R %*% B_vec)

            # stderr
            my_se <- sqrt(diag(R %*% B_vec_vcov %*% t(R)))

            # interindividual differences
            my_sd <- sqrt(diag(R %*% B_var_vec_est %*% t(R)))

            result <- data.frame(
                #effect = paste0(hypothesis$get_description, 1:length(est)),
                effect = hypothesis$get_contrast_names,
                Estimate = est,
                `Std. Error` = my_se,
                `z value` = est/my_se,
                `CI 2.5%` = est + qnorm(0.025) * my_se,
                `CI 97.5%` = est + qnorm(0.975) * my_se,
                SD = my_sd,
                `Estimate - SD` = est - my_sd,
                `Estimate + SD` = est + my_sd,
                `Pr(>|z|)` = pnorm(-abs(est/my_se)) + 1 - pnorm(abs(est/my_se)),
                check.names = F
            )
            pl <- result |>
                ggplot(aes(y = Estimate, x = effect)) +
                theme_classic()

            if ("bar" %in% geom) {
                pl <- pl +
                    geom_bar(stat="identity")
            }
            if ("line" %in% geom) {
                pl <- pl +
                    geom_line(aes(group = 1))
            }
            if ("point" %in% geom) {
                pl <- pl +
                    geom_point()
            }
            if (interval == "sd") {
                pl <- pl +
                    geom_errorbar(
                        aes(ymin = Estimate - SD, ymax = Estimate + SD),
                        width = .2,
                        position = position_dodge(.9)
                    )
            } else if (interval == "ci") {
                pl <- pl +
                    geom_errorbar(
                        aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`),
                        width = .2,
                        position = position_dodge(.9)
                    )
            }
            pl

        }
    )
)

