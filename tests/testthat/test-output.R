manifest_fit <- function() {
    suppressWarnings(semnova(
        data = reading_manifest, id = "id", dv = "re_fix_time",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
}

test_that("anova_table() produces one row per hypothesis with the right degrees of freedom", {
    fit <- manifest_fit()
    tbl <- anova_table(fit)

    expect_s3_class(tbl, "anova")
    expect_equal(rownames(tbl), c("(Intercept)", "grade", "sentence", "grade:sentence"))
    expect_equal(tbl[["Df"]], c(1, 2, 1, 2))
    expect_true(all(tbl[["Chisq"]] >= 0))
    expect_true(all(tbl[["Pr(>Chisq)"]] >= 0 & tbl[["Pr(>Chisq)"]] <= 1))
})

test_that("anova() on a single fit is anova_table()", {
    fit <- manifest_fit()
    expect_equal(anova(fit), anova_table(fit))
})

test_that("anova() on several fits compares them as nested models (scenario 7)", {
    rl <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    mk <- function(level) {
        suppressWarnings(semnova(
            data = rl, id = "id", dv = "dv", indicator = "indicator",
            withins = c("grade", "sentence"), invariance_within = level,
            contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
        ))
    }
    configural <- mk("configural")
    strong <- mk("strong")

    cmp <- anova(configural, strong)
    expect_true(is.data.frame(cmp))
    expect_equal(nrow(cmp), 2L)
    # strong invariance is the more constrained model: more df, worse chisq
    expect_gt(cmp[["Df"]][2], cmp[["Df"]][1])
    expect_gte(cmp[["Chisq"]][2], cmp[["Chisq"]][1])
    # model names must be the caller's expressions, not deparsed lavaan objects
    expect_equal(rownames(cmp), c("configural", "strong"))
    expect_true(all(nchar(rownames(cmp)) < 50))
})

test_that("contrast_table() stacks every hypothesis' per-contrast estimates", {
    fit <- manifest_fit()
    tbl <- contrast_table(fit)

    # the hypothesis each contrast belongs to is in the row label, since a
    # character column would not survive printCoefmat()
    expect_s3_class(tbl, "anova")
    expect_equal(rownames(tbl), c(
        "(Intercept)", "grade: grade1", "grade: grade2", "sentence: sentence1",
        "grade:sentence: grade1:sentence1", "grade:sentence: grade2:sentence1"
    ))
    # 1 + 2 + 1 + 2 contrasts across the four terms
    expect_equal(nrow(tbl), 6L)
    expect_true("Pr(>|z|)" %in% names(tbl))
})

test_that("the contrast table prints as a coefficient table, with p-values", {
    out <- capture.output(print(contrast_table(manifest_fit())))

    expect_match(out[1], "Contrast estimates:")
    expect_true(any(grepl("Pr\\(>\\|z\\|\\)", out)))
    expect_true(any(grepl("Signif. codes", out)))
    # p-values formatted, not printed raw
    expect_true(any(grepl("< 2\\.?2?e-16", out)))
})

test_that("fit_measures() returns NULL for a saturated model and indices otherwise", {
    expect_null(fit_measures(manifest_fit()))

    rl <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    latent <- suppressWarnings(semnova(
        data = rl, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"),
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
    measures <- fit_measures(latent)
    expect_true(all(c("chisq", "df", "cfi", "rmsea") %in% names(measures)))
    expect_gt(measures[["df"]], 0)
})

test_that("print() describes the model and shows the hypothesis tests", {
    fit <- manifest_fit()
    out <- paste(capture.output(print(fit)), collapse = "\n")

    expect_match(out, "semnova fit")
    expect_match(out, "manifest")
    expect_match(out, "occasions\\s+: 6")
    expect_match(out, "grade:sentence")
})

test_that("summary() carries the fit, its tests, and its contrast estimates", {
    fit <- manifest_fit()
    s <- summary(fit)

    expect_s3_class(s, "semnova_summary")
    expect_s3_class(s$anova, "anova")
    expect_true(is.data.frame(s$contrasts))
    out <- paste(capture.output(print(s)), collapse = "\n")
    expect_match(out, "Contrast estimates")
})

test_that("print/summary/anova guard against non-fits", {
    expect_error(anova_table("nope"), "must be an Lgc")
    expect_error(contrast_table("nope"), "must be an Lgc")
    expect_error(fit_measures("nope"), "must be an Lgc")
})

test_that("anova_table() errors clearly when a fit has no hypotheses attached", {
    fit <- manifest_fit()
    bare <- suppressWarnings(lgc(
        data = fit$data, etas = fit$etas_mmodel, c_matrix_within = fit$c_matrix_within
    ))
    expect_error(anova_table(bare), "no hypotheses attached")
})

test_that("plot() resolves a term to its hypothesis and returns a ggplot", {
    skip_if_not_installed("ggplot2")
    fit <- manifest_fit()

    p <- plot(fit, terms = c("grade", "sentence"), interval = "sd")
    expect_s3_class(p, "ggplot")

    p_ci <- plot(fit, terms = "grade", interval = "ci")
    expect_s3_class(p_ci, "ggplot")

    p_h <- plot(fit, hypothesis = fit$hypotheses[[2]], geom = c("bar", "point"), interval = "none")
    expect_s3_class(p_h, "ggplot")
})

test_that("plot() term matching is order-insensitive and errors informatively", {
    skip_if_not_installed("ggplot2")
    fit <- manifest_fit()

    expect_s3_class(plot(fit, terms = c("sentence", "grade")), "ggplot")
    expect_error(plot(fit, terms = "nonexistent"), "no hypothesis matches")
    expect_error(plot(fit), "either `hypothesis` or `terms`")
})

test_that("model_parameters() carries the parsed identity of every labelled row", {
    fit <- suppressWarnings(semnova(
        data = reading_latent[reading_latent$indicator != "re_fix_time", ],
        id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"), invariance_within = "strong",
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
    mp <- model_parameters(fit)

    # the measurement layer is labelled by the engine now, so its rows are
    # identified by index rather than only by variable name
    meas <- mp[mp$type == "measurement" & nzchar(mp$label), ]
    expect_true(nrow(meas) > 0L)
    expect_true(all(!is.na(meas$indicator)))
    expect_equal(unique(meas$semnova_type), "loading")

    # strong within-invariance makes one loading per indicator across all six
    # occasions, and index 0 is how the row says so
    expect_true(all(meas$construct == 0L))
    expect_setequal(unique(meas$indicator), c(1L, 2L))

    # the pi layer keeps its own indices, as before
    expect_true(any(!is.na(mp$pi)))
})

test_that("comparing models reports each model's CFI and RMSEA, and their changes", {
    two <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    fit_at <- function(level) suppressWarnings(semnova(
        data = two, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"), invariance_within = level,
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
    configural <- fit_at("configural")
    weak <- fit_at("weak")
    strong <- fit_at("strong")

    cmp <- anova(configural, weak, strong)
    expect_true(all(c("CFI", "RMSEA", "CFI diff", "RMSEA diff") %in% names(cmp)))

    # the values are each model's own, matched by row rather than by position
    for (nm in c("configural", "weak", "strong")) {
        measures <- lavaan::fitMeasures(get(nm)$sem_obj, c("cfi", "rmsea"))
        expect_equal(cmp[nm, "CFI"], unname(measures[["cfi"]]))
        expect_equal(cmp[nm, "RMSEA"], unname(measures[["rmsea"]]))
    }

    # the changes are relative to the previous, less constrained model
    expect_true(is.na(cmp[1L, "CFI diff"]))
    expect_equal(cmp[["CFI diff"]][-1L], diff(cmp[["CFI"]]))
    expect_equal(cmp[["RMSEA diff"]][-1L], diff(cmp[["RMSEA"]]))

    # lavaan's own RMSEA column is the difference test's, and says so
    expect_true("RMSEA_D" %in% names(cmp))

    # each measure sits next to its own change, between the information
    # criteria and the difference test
    expect_equal(
        names(cmp)[seq_len(9)],
        c("Df", "AIC", "BIC", "RMSEA", "RMSEA diff", "CFI", "CFI diff",
          "Chisq", "Chisq diff")
    )
    # the p-value stays last, which is how print.anova() finds it
    expect_match(names(cmp)[ncol(cmp)], "^Pr\\(")
    out <- capture.output(print(cmp))
    expect_true(any(grepl("Signif. codes", out)))
})

test_that("model rows are matched by name, not by the order they were passed", {
    two <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    fit_at <- function(level) suppressWarnings(semnova(
        data = two, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"), invariance_within = level,
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
    configural <- fit_at("configural")
    strong <- fit_at("strong")

    # lavTestLRT sorts by degrees of freedom, so passing the constrained model
    # first must not misattribute the indices
    reversed <- anova(strong, configural)
    expect_equal(
        reversed["configural", "CFI"],
        unname(lavaan::fitMeasures(configural$sem_obj, "cfi"))
    )
    expect_equal(
        reversed["strong", "RMSEA"],
        unname(lavaan::fitMeasures(strong$sem_obj, "rmsea"))
    )
})


test_that("anova() reports whichever fit indices it is asked for", {
    two <- reading_latent[reading_latent$indicator != "re_fix_time", ]
    fit_at <- function(level) suppressWarnings(semnova(
        data = two, id = "id", dv = "dv", indicator = "indicator",
        withins = c("grade", "sentence"), invariance_within = level,
        contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
    ))
    configural <- fit_at("configural")
    strong <- fit_at("strong")

    # the columns appear in the order asked for, each with its own change
    more <- anova(configural, strong, indices = c("rmsea", "cfi", "srmr"))
    expect_equal(
        names(more)[4:9],
        c("RMSEA", "RMSEA diff", "CFI", "CFI diff", "SRMR", "SRMR diff")
    )
    expect_equal(
        more["strong", "SRMR"],
        unname(lavaan::fitMeasures(strong$sem_obj, "srmr"))
    )

    # a single index, and none at all
    one <- anova(configural, strong, indices = "tli")
    expect_true(all(c("TLI", "TLI diff") %in% names(one)))
    expect_false(any(grepl("^CFI", names(one))))

    bare <- anova(configural, strong, indices = NULL)
    expect_false(any(grepl("^(CFI|RMSEA) ", names(bare))))
    expect_true("RMSEA" %in% names(bare)) # lavaan's own difference-test column

    expect_error(anova(configural, strong, indices = "nope"), "no fit measure called nope")
    expect_error(anova(configural, strong, indices = 1), "must be a character vector")
})