# End-to-end tests for lgc()/semnova() and the estimates pipeline, against
# the package's own bundled data. Structural sanity only -- no oracle yet


quiet_semnova <- function(...) suppressWarnings(semnova(...))

manifest_fit <- function() {
    quiet_semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    )
}

test_that("semnova() fits the manifest within-subjects design and derives one hypothesis per model term", {
    fit <- manifest_fit()

    expect_s3_class(fit, "Semnova")
    expect_s3_class(fit, "Lgc")
    expect_true(lavaan::lavInspect(fit$sem_obj, "converged"))
    expect_equal(
        vapply(fit$hypotheses, function(h) h$description, character(1)),
        c("(Intercept)", "grade", "sentence", "grade:sentence")
    )
})

test_that("each derived hypothesis tests at the right degrees of freedom for a 3x2 design", {
    fit <- manifest_fit()
    dfs <- vapply(fit$hypotheses, function(h) test_hypothesis(fit, h)$test$df, numeric(1))
    names(dfs) <- vapply(fit$hypotheses, function(h) h$description, character(1))

    expect_equal(unname(dfs["grade"]), 2)          # 3 levels -> 2 contrasts
    expect_equal(unname(dfs["sentence"]), 1)       # 2 levels -> 1 contrast
    expect_equal(unname(dfs["grade:sentence"]), 2) # 2 x 1
})

test_that("semnova() handles a between-subjects factor, producing a multigroup fit and the full crossed term set", {
    fit <- quiet_semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = c("grade", "sentence"), betweens = "sex",
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum", sex = "contr.treatment")
    )

    expect_equal(length(fit$group_labels), 2L)
    expect_true(lavaan::lavInspect(fit$sem_obj, "converged"))
    expect_true(all(c("sex", "grade:sex", "grade:sentence:sex") %in%
        vapply(fit$hypotheses, function(h) h$description, character(1))))
})

test_that("semnova() fits a latent 2-indicator DV with a covariate -- the tutorial's scenario 6, with no manual append workaround", {
    rl <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    fit <- quiet_semnova(
        data = rl, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"), covariates = "iq",
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    )

    expect_true(lavaan::lavInspect(fit$sem_obj, "converged"))
    expect_gte(unname(lavaan::fitmeasures(fit$sem_obj)["df"]), 0)
    descriptions <- vapply(fit$hypotheses, function(h) h$description, character(1))
    expect_true(all(c("iq", "grade:iq", "grade:sentence:iq") %in% descriptions))
})

test_that("semnova() guards its arguments strictly", {
    expect_error(semnova(data = "not a df", id = "id", dv = "dv"), "must be a data.frame")
    expect_error(semnova(data = reading_manifest, id = 1, dv = "re_fix_time"), "`id` must be a single character")
    expect_error(
        semnova(data = reading_manifest, id = "id", dv = "re_fix_time", withins = "nope"),
        "column\\(s\\) not present"
    )
    expect_error(
        semnova(data = reading_manifest, id = "id", dv = "re_fix_time", invariance_within = "sloppy"),
        "must be one of"
    )
    # a latent covariate's indicators are measured variables, so a factor
    # there is an error -- unlike a manifest covariate, which gets coded
    expect_error(
        semnova(
            data = reading_manifest, id = "id", dv = "re_fix_time",
            withins = "grade", covariates = list(latent_cov = c("sex", "iq"))
        ),
        "must be numeric"
    )
})

test_that("a categorical covariate is coded, and tested as one term", {
    d <- reading_manifest
    d$iq_group <- cut(d$iq, breaks = 3, labels = c("low", "mid", "high"))

    fit <- suppressWarnings(semnova(
        data = d, id = "id", dv = "re_fix_time",
        withins = c("grade", "sentence"), covariates = c("iq_group", "iq"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
    tab <- anova_table(fit)

    # three levels is one two-df term, not two one-df terms
    expect_true("iq_group" %in% rownames(tab))
    expect_equal(tab["iq_group", "Df"], 2)
    expect_equal(tab["grade:iq_group", "Df"], 4) # 2 df (grade) x 2 df (covariate)
    expect_equal(tab["iq", "Df"], 1) # a numeric covariate is unchanged
    expect_false(any(grepl("iq_group_", rownames(tab)))) # the columns are not terms

    # a two-level factor is a single contrast column, so a one-df term
    fit2 <- suppressWarnings(semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = "grade", covariates = "sex",
        contrasts_arg = list(grade = "contr.sum")
    ))
    expect_equal(anova_table(fit2)["sex", "Df"], 1)
})

test_that("several numeric covariates are several covariates, not one latent construct", {
    d <- reading_manifest
    # a covariate is a property of the case, so it must be constant within id
    d$iq2 <- as.integer(factor(d$id)) %% 7

    fit <- suppressWarnings(semnova(
        data = d, id = "id", dv = "re_fix_time",
        withins = "grade", covariates = c("iq", "iq2"),
        contrasts_arg = list(grade = "contr.sum")
    ))
    expect_setequal(names(fit$covariates_mmodel), c("iq", "iq2"))
    expect_true(all(c("iq", "iq2") %in% rownames(anova_table(fit))))
})

test_that("a covariate or between factor that varies within a case is refused", {
    d <- reading_manifest
    d$occasion_score <- seq_len(nrow(d)) # varies within each person

    expect_error(
        semnova(data = d, id = "id", dv = "re_fix_time", withins = "grade",
                covariates = "occasion_score"),
        "take more than one value within a case"
    )
    expect_error(
        semnova(data = d, id = "id", dv = "re_fix_time", withins = "grade",
                betweens = "sentence"),
        "take more than one value within a case"
    )
})

test_that("expand_categorical_covariates() codes with the contrast it is given", {
    d <- data.frame(g = factor(c("a", "b", "c", "a")), x = 1:4)

    default <- expand_categorical_covariates(d, c("g", "x"), list())
    expect_equal(default$terms$g, c("g_b", "g_c")) # contr.treatment: level names
    expect_equal(default$terms$x, "x") # numeric, left alone
    expect_equal(default$data$g_b, c(0, 1, 0, 0))

    summed <- expand_categorical_covariates(d, "g", list(g = "contr.sum"))
    expect_equal(ncol(summed$data) - ncol(d), 2L)
    expect_equal(summed$data[[summed$terms$g[1]]], c(1, 0, -1, 1))

    expect_error(
        expand_categorical_covariates(data.frame(g = factor(rep("a", 3))), "g", list()),
        "only one level"
    )
})

test_that("lgc() guards its arguments strictly", {
    expect_error(lgc(data = reading_manifest, etas = list(a = "re_fix_time"), c_matrix_within = "nope"), "numeric matrix")
    expect_error(
        lgc(data = reading_manifest, etas = list(a = "nonexistent_column"), c_matrix_within = matrix(1)),
        "column\\(s\\) not present"
    )
    expect_error(
        lgc(data = reading_manifest, etas = list(a = c("re_fix_time", "fix_count"), b = "ini_land_pos"), c_matrix_within = matrix(1)),
        "same number of indicators"
    )
})

test_that("model_parameters() classifies parameter types structurally, including semTools-labelled measurement rows", {
    fit <- manifest_fit()
    mp <- model_parameters(fit)

    expect_true(all(c("lhs", "op", "rhs", "group", "label", "est", "se", "free", "type") %in% names(mp)))
    expect_true("measurement" %in% mp$type)
    expect_true(any(mp$semnova_type == "pi_intercept", na.rm = TRUE))

    only_var <- model_parameters(fit, what = "variance")
    expect_true(all(only_var$type == "variance"))
})

test_that("regression_coefficients() builds a (covariate+1) x pi x group array wired to the same labels the syntax used", {
    fit <- manifest_fit()
    rc <- regression_coefficients(fit)

    n_pis <- length(fit$pis)
    expect_equal(dim(rc$B_array), c(1L, n_pis, 1L))
    expect_equal(length(rc$B_vec), n_pis)
    expect_equal(dim(rc$B_vec_vcov), c(n_pis, n_pis))
    expect_equal(names(rc$B_vec), as.vector(rc$labels))
    # every label came from param_labels(), i.e. is a semnova label
    expect_true(all(vapply(names(rc$B_vec), function(l) !is.na(parse_param_label(l)$type), logical(1))))
})

test_that("regression_coefficients() B_var_vec is the pi between-person (co)variance, zero off-group", {
    fit <- manifest_fit()
    rc <- regression_coefficients(fit)
    # single group, all positions are pi intercepts -> fully populated, symmetric
    expect_true(all(rc$B_var_vec != 0))
    expect_equal(rc$B_var_vec, t(rc$B_var_vec))
})

test_that("contrast_estimates() returns per-contrast estimates plus an omnibus Wald test", {
    fit <- manifest_fit()
    h <- fit$hypotheses[[which(vapply(fit$hypotheses, function(x) x$description, character(1)) == "grade")]]
    res <- contrast_estimates(fit, h)

    expect_s3_class(res, "semnova_contrast")
    expect_equal(nrow(res$table), 2L) # 2 grade contrasts
    # a coefficient table: contrasts are row labels, and the statistic and
    # p-value columns are named so printCoefmat() formats them
    expect_true(all(c("estimate", "std_error", "z value", "ci_lower", "ci_upper",
                      "sd_between", "Pr(>|z|)") %in% names(res$table)))
    expect_equal(rownames(res$table), c("grade1", "grade2"))
    expect_s3_class(res$table, "anova")
    expect_true(all(res$table$std_error > 0))
    expect_true(all(res$table$sd_between >= 0))
    expect_equal(res$test$df, 2L)
    expect_gte(res$test$p.value, 0)
    expect_lte(res$test$p.value, 1)
})

test_that("test_hypothesis() and contrast_estimates() are the same computation", {
    fit <- manifest_fit()
    h <- fit$hypotheses[[2]]
    expect_equal(test_hypothesis(fit, h), contrast_estimates(fit, h))
})

test_that("a hand-built custom hypothesis goes through exactly the same path as a derived one (scenario 8)", {
    fit <- manifest_fit()
    # contrast the first within-subject contrast against zero, by hand
    M <- matrix(0, nrow = length(fit$pis), ncol = 1)
    M[2, 1] <- 1
    custom <- hypothesis(M = M, description = "custom: second pi")

    res <- test_hypothesis(fit, custom)
    expect_s3_class(res, "semnova_contrast")
    expect_equal(nrow(res$table), 1L)
    expect_equal(res$test$df, 1L)
})

test_that("contrast_estimates() rejects a hypothesis whose matrices don't fit the model", {
    fit <- manifest_fit()
    bad <- hypothesis(M = matrix(1, nrow = 2, ncol = 1)) # wrong number of etas
    expect_error(contrast_estimates(fit, bad), "nrow\\(hypothesis\\$M\\)")
})

test_that("contrast_estimates() guards its arguments", {
    fit <- manifest_fit()
    expect_error(contrast_estimates("not a fit", hypothesis()), "must be an Lgc")
    expect_error(contrast_estimates(fit, "not a hypothesis"), "must be a Hypothesis")
    expect_error(contrast_estimates(fit, fit$hypotheses[[1]], level = 1.5), "between 0 and 1")
})

test_that("lgc() reaches the same fitted model as semnova() for an equivalent manifest specification", {
    sem_fit <- manifest_fit()

    wide <- sem_fit$data
    etas <- sem_fit$etas_mmodel
    low_fit <- suppressWarnings(lgc(
        data = wide,
        etas = etas,
        c_matrix_within = sem_fit$c_matrix_within
    ))

    expect_equal(
        unname(lavaan::fitmeasures(low_fit$sem_obj)["df"]),
        unname(lavaan::fitmeasures(sem_fit$sem_obj)["df"])
    )
    expect_equal(
        unname(regression_coefficients(low_fit)$B_vec),
        unname(regression_coefficients(sem_fit)$B_vec),
        tolerance = 1e-6
    )
})

test_that("the covariance-structure options constrain what they claim to", {
    fit <- function(...) suppressWarnings(semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum"), ...
    ))
    df_of <- function(f) unname(lavaan::fitMeasures(f$sem_obj, "df"))

    # unstructured: 6 contrast variables, 21 free (co)variances, saturated
    expect_equal(df_of(fit()), 0)

    # sphericity is stated in the design's terms, which is what semnova() adds
    # over lgc(): "grade" spans 2 contrast variables, so equating their
    # variances and zeroing their covariance costs 2 df
    expect_equal(df_of(fit(sphericity = "grade")), 2)
    expect_equal(df_of(fit(sphericity = "grade:sentence")), 2)
    # TRUE takes every term that spans more than one contrast variable
    # (grade and grade:sentence; sentence spans only one)
    expect_equal(df_of(fit(sphericity = TRUE)), 4)
    # an interaction may be given as its factors, in any order
    expect_equal(df_of(fit(sphericity = list(c("sentence", "grade")))), 2)
    # and lgc()'s index sets still pass through untranslated
    expect_equal(df_of(fit(sphericity = list(2:6))), 14)

    # compound symmetry: all 21 replaced by one variance and one covariance
    expect_equal(df_of(fit(compound_symmetry = TRUE)), 19)

    # and the estimates still come out: the ANOVA table is unaffected in shape
    expect_equal(nrow(anova_table(fit(sphericity = TRUE))), nrow(anova_table(fit())))
})

test_that("variance homogeneity and stochastic weights work on a multigroup fit", {
    fit <- function(...) suppressWarnings(semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = c("grade", "sentence"), betweens = "sex",
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum", sex = "contr.treatment"),
        ...
    ))
    df_of <- function(f) unname(lavaan::fitMeasures(f$sem_obj, "df"))

    expect_equal(df_of(fit()), 0)
    expect_equal(df_of(fit(variance_homogeneity = TRUE)), 21) # one set of 21 for both groups

    weighted <- fit(group_weights = "stochastic")
    pt <- lavaan::parTable(weighted$sem_obj)
    expect_true(any(pt$op == "%"))

    # the estimated frequencies should recover the observed group proportions --
    # of the sample actually analysed, which listwise deletion makes smaller
    # than the raw one (267 people, 99 of them with a missing occasion)
    rel <- pt$est[pt$lhs %in% c("relfreq1", "relfreq2") & pt$op == ":="]
    n_used <- unlist(lavaan::lavInspect(weighted$sem_obj, "nobs"))
    expect_equal(sort(rel), sort(n_used / sum(n_used)), tolerance = 1e-6, ignore_attr = TRUE)

    expect_false(any(lavaan::parTable(fit()$sem_obj)$op == "%")) # "fixed" adds nothing
})

test_that("semnova() guards the covariance-structure arguments", {
    call <- function(...) semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time", withins = "grade", ...
    )
    expect_error(call(compound_symmetry = "yes"), "must be TRUE or FALSE")
    expect_error(call(variance_homogeneity = NA), "must be TRUE or FALSE")
    expect_error(call(group_weights = "random"), "'arg' should be one of")
    expect_error(call(sphericity = list(c(1, 99))), "outside the design")
})


test_that("sphericity terms resolve to the contrast variables they span", {
    design <- parse_design(
        reading_manifest, c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    )
    resolve <- function(x) semnova:::resolve_sphericity_terms(x, design)

    # (Intercept) | grade grade | sentence | grade:sentence grade:sentence
    expect_equal(resolve("grade"), list(2:3))
    expect_equal(resolve("grade:sentence"), list(5:6))
    expect_equal(resolve(c("grade", "grade:sentence")), list(2:3, 5:6))

    # an interaction by its factors, either order, is the same term
    expect_equal(resolve(list(c("grade", "sentence"))), list(5:6))
    expect_equal(resolve(list(c("sentence", "grade"))), list(5:6))

    # TRUE means every term wide enough to constrain; a one-column term
    # (sentence) and the intercept have no covariance to zero
    expect_equal(resolve(TRUE), list(2:3, 5:6))
    expect_equal(resolve("sentence"), list())

    # nothing, and the low-level vocabulary, both still work
    expect_equal(resolve(FALSE), list())
    expect_equal(resolve(list()), list())
    expect_equal(resolve(list(2:4)), list(2:4))
    expect_equal(resolve(list(c(".pi2", ".pi3"))), list(c(".pi2", ".pi3")))

    expect_error(resolve("colour"), "no model term matches colour")
    expect_error(resolve("colour"), "Available terms: \\(Intercept\\), grade")
    expect_error(resolve(42), "must be TRUE, a model term")
})
test_that("residual covariances are named by indicator, not by generated column", {
    two <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    fit <- function(...) suppressWarnings(semnova(
        data = two, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum"), ...
    ))
    df_of <- function(f) unname(lavaan::fitMeasures(f$sem_obj, "df"))

    base <- df_of(fit())
    # one indicator across 6 occasions: 15 pairs, freely estimated
    expect_equal(df_of(fit(resid_cov = list("fix_count"))), base - 15)
    # the same 15 pairs constrained equal cost one parameter, not fifteen
    expect_equal(df_of(fit(resid_cov = resid_set("fix_count", equal = TRUE))), base - 1)

    expect_error(fit(resid_cov = list("nope")), "no indicator called nope")
    expect_error(fit(resid_cov = list("nope")), "Indicators: fix_count, ini_land_pos")
})

test_that("resid_set() resolves indicators and conditions to model variables", {
    etas <- list(
        .eta1 = c("a_t1", "b_t1"),
        .eta2 = c("a_t2", "b_t2"),
        .eta3 = c("a_t3", "b_t3")
    )
    idata <- data.frame(time = c("t1", "t2", "t3"), half = c("early", "early", "late"))
    resolve <- function(x, default_equal = FALSE) {
        semnova:::resolve_resid_sets(x, etas, c("a", "b"), idata, "resid_cov", default_equal)
    }
    variables <- function(x, ...) lapply(resolve(x, ...), function(s) s$variables)

    # an indicator expands across every occasion
    expect_equal(variables(list(resid_set("a"))), list(c("a_t1", "a_t2", "a_t3")))
    # omitting the indicator takes them all, in one set
    expect_equal(variables(TRUE), list(c("a_t1", "a_t2", "a_t3", "b_t1", "b_t2", "b_t3")))
    # conditions restrict which occasions are in it
    expect_equal(variables(list(resid_set("a", half = "early"))), list(c("a_t1", "a_t2")))
    expect_equal(variables(list(resid_set(time = c("t1", "t3")))),
                 list(c("a_t1", "a_t3", "b_t1", "b_t3")))

    # the shorthand still works, and so does naming variables directly
    expect_equal(variables(list("a")), list(c("a_t1", "a_t2", "a_t3")))
    expect_equal(variables(list(c("a_t1", "a_t2"))), list(c("a_t1", "a_t2")))

    # `equal` follows the argument unless the set states it
    expect_false(resolve(list(resid_set("a")))[[1]]$equal) # resid_cov: free
    expect_true(resolve(list(resid_set("a")), default_equal = TRUE)[[1]]$equal)  # as resid_var does
    expect_true(resolve(list(resid_set("a", equal = TRUE)))[[1]]$equal)

    expect_equal(variables(list()), list())
    expect_error(resolve(list(resid_set("c"))), "no indicator called c")
    expect_error(resolve(list(resid_set("a", colour = "red"))), "no within-subject factor called colour")
    expect_error(resolve(list(resid_set("a", half = "middle"))), "half has no level middle")
    expect_error(resolve(42), "must be TRUE, a resid_set")

    # a set covering fewer than two terms constrains nothing
    expect_warning(out <- resolve(list(resid_set("a", time = "t1"))), "fewer than two")
    expect_equal(out, list())
})

test_that("resid_set() validates what it is given, where it is written", {
    expect_error(resid_set(1), "must be a character vector")
    expect_error(resid_set("a", "landolt"), "must be named by the factor")
    expect_error(resid_set("a", grade = "g1", grade = "g2"), "named more than once")
    expect_error(resid_set("a", grade = character(0)), "no levels given")
    expect_error(resid_set("a", equal = "yes"), "must be TRUE or FALSE")

    set <- resid_set(c("a", "b"), grade = c("g1", "g2"))
    expect_s3_class(set, "ResidSet")
    expect_equal(set$indicator, c("a", "b"))
    expect_equal(set$conditions, list(grade = c("g1", "g2")))
    expect_null(set$equal) # decided by the argument it is passed to

    expect_output(print(resid_set("a", grade = "g1")), "<resid_set> a | grade = g1", fixed = TRUE)
    expect_output(print(resid_set()), "every indicator", fixed = TRUE)
})

test_that("residual structure needs a latent dv, and a set needs two terms", {
    expect_warning(
        manifest <- semnova(
            data = reading_manifest, id = "id", dv = "re_fix_time",
            withins = c("grade", "sentence"), resid_var = TRUE,
            contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
        ),
        "need a latent dependent variable"
    )
    expect_s3_class(manifest, "Semnova")

    # one occasion: an indicator appears once, so there is nothing to equate
    one <- reading_latent[reading_latent$indicator != "re_fix_time" &
        reading_latent$grade == "grade1" & reading_latent$sentence == "sentence", ]
    expect_warning(
        semnova(data = one, id = "id", dv = "dv", indicator = "indicator",
                resid_var = resid_set("fix_count")),
        "fewer than two residual terms"
    )
})

test_that("resid_var equates the residual variances it selects", {
    two <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    fit <- function(...) suppressWarnings(semnova(
        data = two, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum"), ...
    ))
    df_of <- function(f) unname(lavaan::fitMeasures(f$sem_obj, "df"))
    base <- df_of(fit())

    # 12 residual variances (2 indicators x 6 occasions) collapsed into one
    expect_equal(df_of(fit(resid_var = TRUE)), base + 11)
    # one indicator across 6 occasions
    expect_equal(df_of(fit(resid_var = resid_set("fix_count"))), base + 5)
    # ... restricted to the 3 landolt occasions
    expect_equal(df_of(fit(resid_var = resid_set("fix_count", sentence = "landolt"))), base + 2)
    # two sets, one per indicator, are not the same as one pooled set
    expect_equal(
        df_of(fit(resid_var = list(resid_set("fix_count"), resid_set("ini_land_pos")))),
        base + 10
    )
})

test_that("FALSE, NULL and list() all mean 'no structure', and TRUE means all of it", {
    two <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    fit <- function(...) suppressWarnings(semnova(
        data = two, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum"), ...
    ))
    df_of <- function(f) unname(lavaan::fitMeasures(f$sem_obj, "df"))
    base <- df_of(fit())

    # the three spellings of "nothing" agree, on every structure argument
    for (nothing in list(FALSE, NULL, list())) {
        expect_equal(df_of(fit(resid_cov = nothing)), base)
        expect_equal(df_of(fit(resid_var = nothing)), base)
        expect_equal(df_of(fit(sphericity = nothing)), base)
    }

    # and FALSE is what the defaults are, so an explicit FALSE changes nothing
    expect_equal(names(formals(semnova))[
        names(formals(semnova)) %in% c("resid_cov", "resid_var", "sphericity")
    ], c("resid_cov", "resid_var", "sphericity"))
    for (arg in c("resid_cov", "resid_var", "sphericity")) {
        expect_false(eval(formals(semnova)[[arg]]))
    }
})

test_that("lgc() takes the same FALSE, and TRUE means every contrast variable", {
    pis <- paste0(".pi", 1:4)
    expect_equal(semnova:::check_sphericity(FALSE, pis), list())
    expect_equal(semnova:::check_sphericity(NULL, pis), list())
    expect_equal(semnova:::check_sphericity(TRUE, pis), list(1:4))
    expect_false(eval(formals(lgc)$sphericity))
})

test_that("strict invariance wins over resid_var, and says so", {
    two <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    fit <- function(...) semnova(
        data = two, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum"), ...
    )
    df_of <- function(f) unname(lavaan::fitMeasures(f$sem_obj, "df"))

    strict_only <- suppressWarnings(fit(invariance_within = "strict"))

    # every kind of resid_var set gives the strict model back, unchanged: one
    # implied by strict, one going beyond it, and one that used to *undo* it
    # by relabelling a subset of occasions (df 59 -> 58 before this guard)
    for (sets in list(
        resid_set("fix_count"),
        TRUE,
        resid_set(grade = "grade1", sentence = "sentence")
    )) {
        expect_warning(
            got <- fit(invariance_within = "strict", resid_var = sets),
            "already constrains residual variances"
        )
        expect_equal(df_of(got), df_of(strict_only))
    }

    # invariance_between = "strict" is the same conflict, across groups
    expect_warning(
        semnova(data = two, id = "id", dv = "dv", indicator = "indicator",
                withins = c("grade", "sentence"), betweens = "sex",
                invariance_between = "strict", resid_var = resid_set("fix_count"),
                contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum",
                                     sex = "contr.treatment")),
        "invariance_between"
    )

    # residual *covariances* are a different parameter, so no conflict
    expect_silent(fit(invariance_within = "strict", resid_cov = resid_set("fix_count")))
    expect_lt(
        df_of(suppressWarnings(fit(invariance_within = "strict", resid_cov = resid_set("fix_count")))),
        df_of(strict_only)
    )

    # and below strict, a resid_var set does what it says
    expect_gt(
        df_of(suppressWarnings(fit(invariance_within = "strong", resid_var = resid_set("fix_count")))),
        df_of(suppressWarnings(fit(invariance_within = "strong")))
    )
})

test_that("resid_var sets may not overlap, since one would overwrite the other", {
    expect_error(
        build_resid_var_syntax(list(c("a", "b"), c("b", "c"))),
        "appear in more than one set"
    )
    expect_silent(build_resid_var_syntax(list(c("a", "b"), c("c", "d"))))
})
