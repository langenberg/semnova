# Enforces the convention that the vignette
# is the feature inventory"): every exported, user-facing object must appear
# in vignettes/semnova.Rmd. This is what makes the vignette stay in sync
# automatically -- adding an export without documenting it fails the suite,
# rather than being noticed months later.

# Exports deliberately not shown in the tutorial, each with a reason. Add to
# this list only when an export genuinely isn't something a user would call.
vignette_exempt <- c(
    # R6 generators: users construct via lgc()/semnova()/hypothesis(), which
    # the vignette does cover. The generators are exported only so that
    # class(fit) and inheritance work for downstream code.
    "Lgc", "Semnova", "Hypothesis", "ResidSet"
)

vignette_path <- function() {
    # works both from tests/testthat/ (interactive) and from the installed
    # package's test directory
    candidates <- c(
        testthat::test_path("..", "..", "vignettes", "semnova.Rmd"),
        testthat::test_path("..", "..", "..", "vignettes", "semnova.Rmd")
    )
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0L) NA_character_ else hit[[1L]]
}

test_that("every exported user-facing object appears in the vignette", {
    path <- vignette_path()
    skip_if(is.na(path), "vignette source not available (installed package)")

    exports <- getNamespaceExports("semnova")
    # S3 methods are exercised through their generics (print/plot/anova/summary),
    # which the vignette does use; they have no standalone call site.
    exports <- grep("\\.(Lgc|Semnova|semnova_contrast|semnova_summary)$", exports,
        value = TRUE, invert = TRUE
    )
    expected <- setdiff(sort(exports), vignette_exempt)

    vignette_text <- paste(readLines(path, warn = FALSE), collapse = "\n")

    missing <- expected[!vapply(expected, function(fn) {
        grepl(paste0("\\b", gsub(".", "\\.", fn, fixed = TRUE), "\\b"), vignette_text, perl = TRUE)
    }, logical(1L))]

    expect_equal(
        missing, character(0),
        info = paste0(
            "These exports are missing from vignettes/semnova.Rmd: ",
            paste(missing, collapse = ", "),
            ". Document them there, or add them to `vignette_exempt` with a reason."
        )
    )
})

test_that("the vignette documents the known capability gaps", {
    path <- vignette_path()
    skip_if(is.na(path), "vignette source not available (installed package)")

    vignette_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    # deferred work is stated in the vignette so users aren't left guessing
    # why a feature they expected is absent
    gaps <- sub(".*# What is not here yet", "", vignette_text)
    expect_match(gaps, "Power analysis")
    expect_match(gaps, "Marginal effects")

    # and what is no longer a gap is not listed as one: sphericity and
    # categorical covariates were implemented in 0.3.0
    expect_no_match(gaps, "[Ss]phericity")
    expect_no_match(gaps, "[Cc]ategorical covariates")
})
