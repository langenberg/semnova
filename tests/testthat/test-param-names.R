test_that("param_label() produces the documented fixed format", {
    expect_equal(param_label("loading", construct = 1, indicator = 2, group = 1), ".lambda_1_2_1")
    expect_equal(param_label("intercept", construct = 1, indicator = 2, group = 1), ".nu_1_2_1")
    expect_equal(param_label("resid_var", construct = 1, indicator = 2, group = 1), ".theta_1_2_1")
    expect_equal(param_label("resid_cov", construct = 1, indicator = 2, indicator2 = 3, group = 1), ".theta_1_2_3_1")
    expect_equal(param_label("latent_mean", construct = 2, group = 1), ".alpha_2_1")
    expect_equal(param_label("latent_var", construct = 2, group = 1), ".psi_2_1")
    expect_equal(param_label("latent_cov", construct = 2, construct2 = 3, group = 1), ".psi_2_3_1")
    expect_equal(param_label("pi_intercept", pi = 1, group = 2), ".alpha_pi_1_2")
    expect_equal(param_label("pi_var", pi = 1, group = 2), ".psi_pi_1_2")
    expect_equal(param_label("pi_cov", pi = 1, pi2 = 2, group = 2), ".psi_pi_1_2_2")
    expect_equal(param_label("regression", pi = 1, covariate = 2, group = 1), ".beta_1_2_1")
    expect_equal(param_label("group_weight", group = 3), ".gw_3")
})

test_that("param_label() errors on unknown type or missing index", {
    expect_error(param_label("nonsense", construct = 1), "unknown parameter type")
    expect_error(param_label("loading", construct = 1), "missing an index")
})

test_that("parse_param_label() is a true inverse of param_label()", {
    cases <- list(
        list(type = "loading", construct = 1, indicator = 2, group = 3),
        list(type = "intercept", construct = 1, indicator = 2, group = 3),
        list(type = "resid_var", construct = 1, indicator = 2, group = 3),
        list(type = "resid_cov", construct = 1, indicator = 2, indicator2 = 4, group = 3),
        list(type = "latent_mean", construct = 5, group = 1),
        list(type = "latent_var", construct = 5, group = 1),
        list(type = "latent_cov", construct = 5, construct2 = 6, group = 1),
        list(type = "pi_intercept", pi = 2, group = 1),
        list(type = "pi_var", pi = 2, group = 1),
        list(type = "pi_cov", pi = 2, pi2 = 3, group = 1),
        list(type = "regression", pi = 2, covariate = 1, group = 1),
        list(type = "group_weight", group = 4)
    )

    for (case in cases) {
        label <- do.call(param_label, case)
        parsed <- parse_param_label(label)
        expect_equal(parsed, case, info = label)
    }
})

test_that("parse_param_label() returns NA type for a label it didn't produce", {
    expect_true(is.na(parse_param_label("Y1_grade1_sentence")$type))
    expect_true(is.na(parse_param_label(".foo_1_2")$type))
})

test_that("regression_labels() has the (covariate x pi x group) shape B is filled from", {
    arr <- regression_labels(pis = 1:2, covariates = c("iq", "age"), groups = c("g1", "g2"))
    expect_equal(dim(arr), c(3L, 2L, 2L))
    expect_equal(dimnames(arr)$covariate, c("(Intercept)", "iq", "age"))
    expect_equal(arr["(Intercept)", "1", "g1"], param_label("pi_intercept", pi = 1, group = 1))
    expect_equal(arr["iq", "1", "g1"], param_label("regression", pi = 1, covariate = 1, group = 1))
    expect_equal(arr["age", "2", "g2"], param_label("regression", pi = 2, covariate = 2, group = 2))
})

test_that("regression_labels() works without covariates and without groups", {
    intercepts_only <- regression_labels(pis = 1:3)
    expect_equal(dim(intercepts_only), c(1L, 3L, 1L))
    expect_equal(dimnames(intercepts_only)$covariate, "(Intercept)")
    expect_equal(dimnames(intercepts_only)$group, "1")
    expect_equal(intercepts_only[1, 2, 1], param_label("pi_intercept", pi = 2, group = 1))
})

test_that("regression_labels() positions match what regression_coefficients() fills", {
    fit <- suppressWarnings(semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = "grade", covariates = "iq",
        contrasts_arg = list(grade = "contr.sum")
    ))
    coefs <- regression_coefficients(fit)

    # the array of estimates and the array of labels are the same shape, from
    # the same call -- that agreement is the reason this enumerator exists
    expect_equal(dim(coefs$B_array), dim(coefs$labels))
    expect_equal(dimnames(coefs$B_array), dimnames(coefs$labels))
    expect_equal(
        coefs$labels,
        regression_labels(pis = fit$pis, covariates = "iq", groups = fit$group_labels)
    )
})

test_that("pretty_label() format='syntax' is the identity function", {
    label <- param_label("loading", construct = 1, indicator = 2, group = 1)
    expect_equal(pretty_label(label, format = "syntax"), label)
})

test_that("pretty_label() falls back to the raw label for unrecognized input", {
    expect_equal(pretty_label("Y1_grade1_sentence"), "Y1_grade1_sentence")
})

test_that("pretty_label() format='text' renders a readable subscript form", {
    label <- param_label("loading", construct = 1, indicator = 2, group = 1)
    expect_equal(pretty_label(label, format = "text"), "lambda_{1,2,1}")
})

test_that("n_occasions() is 1 for covariates, else the shared within-design count", {
    expect_equal(n_occasions("iq", n_within_occasions = 6, covariate_names = c("iq", "age")), 1L)
    expect_equal(n_occasions("eta1", n_within_occasions = 6, covariate_names = c("iq", "age")), 6L)
    expect_equal(n_occasions("eta1", n_within_occasions = 1, covariate_names = character()), 1L)
})

test_that("pretty_label() format='plotmath' renders semnova and foreign labels alike", {
    # semnova's own labels go through parse_param_label(), so the pi layer keeps
    # the marker that separates a pi intercept from a latent mean
    expect_equal(pretty_label(".alpha_pi_1_1", format = "plotmath"), "alpha['pi'*','*1*','*1]")
    expect_equal(pretty_label(".lambda_1_2_1", format = "plotmath"), "lambda[1*','*2*','*1]")
    # the measurement layer carries semTools' format instead
    expect_equal(pretty_label("lambda.1_1", format = "plotmath"), "lambda[1*','*1]")
    expect_equal(pretty_label(".nu_y__1_", format = "plotmath"), "nu['y'*','*'_'*','*1]")

    # vectorised, and empty for anything that is not maths -- a caller about to
    # parse() the result has no use for a raw variable name
    expect_equal(pretty_label(c("", "Y1_grade1", ".psi_pi_2_1"), format = "plotmath"),
                 c("", "", "psi['pi'*','*2*','*1]"))

    for (lab in c(".alpha_pi_1_1", ".theta_1_2_3_1", ".beta_2_1_1", "nu.3")) {
        expect_silent(parse(text = pretty_label(lab, format = "plotmath")))
    }
})

test_that("pretty_label() format='text' marks the pi layer too", {
    expect_equal(pretty_label(".alpha_pi_1_1", format = "text"), "alpha_{pi,1,1}")
    expect_equal(pretty_label(".alpha_1_1", format = "text"), "alpha_{1,1}")
})

test_that("param_label() renders a parameter from its identity, not from a label string", {
    # the identity is the argument, and all three renderings come from it --
    # no caller should have to build a label and hand it back to be re-parsed
    expect_equal(param_label("loading", construct = 1, indicator = 2, group = 1),
                 ".lambda_1_2_1")
    expect_equal(param_label("loading", construct = 1, indicator = 2, group = 1, format = "plotmath"),
                 "lambda[1*','*2*','*1]")
    expect_equal(param_label("loading", construct = 1, indicator = 2, group = 1, format = "text"),
                 "lambda_{1,2,1}")
    expect_equal(param_label("pi_intercept", pi = 1, group = 1, format = "text"), "alpha_{pi,1,1}")
    expect_error(param_label("nonsense", group = 1, format = "text"), "unknown parameter type")
})

test_that("the string and identity paths agree for semnova's own labels", {
    ids <- list(
        list(type = "loading", construct = 3, indicator = 1, group = 2),
        list(type = "resid_cov", construct = 1, indicator = 2, indicator2 = 3, group = 1),
        list(type = "pi_cov", pi = 1, pi2 = 4, group = 2),
        list(type = "regression", pi = 2, covariate = 1, group = 1)
    )
    for (id in ids) {
        label <- do.call(param_label, id)
        for (fmt in c("text", "plotmath")) {
            expect_equal(
                pretty_label(label, format = fmt),
                do.call(param_label, c(id, list(format = fmt)))
            )
        }
    }
})
