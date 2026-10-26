# semnova 0.3.0.9001

## Bug fixes

* `dag()` now draws the arrow from each residual (epsilon) node to its
  indicator whenever residuals are shown. With `what = "resid_cov"` and no
  `"resid_var"`, the epsilon nodes were drawn without these arrows.

* `dag()` bends covariance arcs away from the centre of the diagram. The side
  used to depend on the order of the pair in the model, so residual
  covariance arcs could bend into the indicator column. The default bend
  angle is now 45 degrees instead of 18, which separates nested arcs.

* `dag()` draws arcs correctly when `covariance_scale` puts the bend angle
  at 90 degrees or more. Such arcs were drawn on the wrong side.

# semnova 0.3.0

A complete rewrite of the package's internals. What the package can do is
preserved and extended; how it is organised, and much of its API, is not.
`lgc()`, `semnova()`, `plot()` and `dag()` keep their names. Their arguments
have changed.

Two vignettes document the result: `vignette("semnova")` covers every feature
the package has, and `vignette("glh")` derives the general linear hypothesis
that every test is built from.

## Breaking changes

* `semnova()` and `lgc()` have new signatures. `semnova()` now states the
  residual and covariance structure in the design's own vocabulary: model
  terms, indicators and conditions, rather than contrast-variable positions or
  generated column names.

* `semnova(covariates = c("iq", "age"))` now means **two covariates**. It
  previously meant one latent covariate measured by two indicators, which
  contradicted the argument's own documentation and silently fitted a different
  model from the one described. `lgc()`'s reading of a character vector, where
  you describe constructs directly, is unchanged.

* `equal_resid_cov` is gone from `semnova()`; residual covariances are now
  described with `resid_set(..., equal = TRUE)`. The argument survives on
  `lgc()`, whose vocabulary is model variables.

* The per-contrast tables from `contrast_estimates()` and `contrast_table()`
  are now coefficient tables of class `"anova"`, so they print with formatted
  p-values and significance codes. The contrast (and, in `contrast_table()`,
  the hypothesis) moves from a column into the row label, and the `z` and
  `pvalue` columns are renamed `z value` and `Pr(>|z|)`, the names
  `printCoefmat()` recognises. Index those with backticks. A character column
  could not be kept, since that formatting converts one to factor codes.

* `param_labels()` is renamed `regression_labels()` and reduced to the one
  family it enumerates: the pi-layer regression coefficients, as the array
  `regression_coefficients()` fills. Its `loading`/`intercept`/`resid_var`
  branches are removed; use `param_label()` for those, one label at a time.

* `dag()` is a new implementation on `ggraph`. Nodes stay circular and curved
  paths keep their bend at any figure size, and the roughly twenty-five layout
  arguments of the old version are gone. It labels every parameter it draws by
  default (`labels = "all"`), with fixed ones inside a ring accent;
  `labels = "free"` restores the sparser view.

* `dag_semplot()` and `dag_layout()` (the `semPlot` rendering) are retired: the
  code remains in the package but is no longer exported.

* **Power analysis is not included in this release.** `power_analysis_lgc()`,
  `power_analysis_semnova()`, `power_script_lgc()`, `power_script_semnova()`,
  the `Power` class and its `plot()`/`summary()` methods are all removed. This
  is a deferral, not a removal on principle; the capability is planned to
  return.

* Marginal effects are not included. `Effect.Semnova()`, `SemnovaEffect` and
  the surrounding methods relied on an unexported internal of the `effects`
  package and on making a fit impersonate an `lm`. `plot()` on a hypothesis
  covers similar ground.

* The `ContrastPlot`, `Dag`, `ParTable` and `Options` classes are gone. They
  held no state beyond a reference to a fit, and are replaced by plain
  functions that take the fit as an argument.

* Removed alongside them: `lgc_dummy()`, `semnova_dummy()`, `reliabilities()`,
  and the `SemnovaFormula` methods.

## New features

### Model specification

* **Categorical covariates.** A factor covariate is coded into contrast columns
  (`contrasts_arg[[name]]`, else `contr.treatment`) and its columns are tested
  **jointly**: a three-level covariate is one effect with two degrees of
  freedom, not two effects.

* **Residual structure**, described by indicator and condition with the new
  `resid_set()`: `resid_var` constrains residual variances equal,
  `resid_cov` adds residual covariances, equal or free. `TRUE` covers
  everything. A set that cannot apply is skipped with a warning that says why.
  That covers a manifest dependent variable, a set of fewer than two residual
  terms, and `"strict"` invariance, which already equates residual variances. Overlapping
  `resid_var` sets are an error, since one would silently overwrite the other.

* **Covariance structure of the contrast layer**: `sphericity` (stated as model
  terms, e.g. `TRUE`, `"grade"`, `"grade:sentence"`), `compound_symmetry`,
  `variance_homogeneity`, and `group_weights = "stochastic"` for models where
  group membership is itself random.

* **Measurement invariance** across occasions and groups is delegated to
  `semTools::measEq.syntax()`. semnova relabels its output, so a fitted model
  carries one label namespace.

### Results

* A single, consolidated path from a fit to its estimates:
  `model_parameters()` reads the fitted model, `regression_coefficients()`
  assembles the coefficient array, and `contrast_estimates()` (also
  `test_hypothesis()`) tests a hypothesis against it. An ANOVA row, a custom
  hypothesis and a contrast plot are the same code with different matrices.

* `hypothesis()` constructs a general linear hypothesis by hand;
  `generate_anova_hypotheses()` builds one per model term.

* `anova()` on a single fit gives the hypothesis tests. On several fits it
  compares them as nested models, and reports each model's `RMSEA` and `CFI`
  plus the change in each from the previous model. A difference test alone
  rejects nearly any constraint in a large sample. `indices` takes any
  measure `lavaan::fitMeasures()` knows, in the order the columns should
  appear. lavaan's own `RMSEA` column, being the RMSEA of the difference test
  rather than of either model, is shown as `RMSEA_D`. Also `anova_table()`, `contrast_table()`,
  `fit_measures()`, and `summary()`.

* A parameter naming engine: `param_label()` builds a label from a parameter's
  identity in any of three renderings (lavaan syntax, readable text, plotmath),
  `parse_param_label()` inverts it, and `pretty_label()` reads labels back off
  a fitted model. Labels state their own equality constraints: an index of `0`
  means one parameter shared across that dimension.

### Diagrams

* `dag_save()` saves a path diagram and crops the whitespace around it, using
  `magick` where available and saying so when not.

* `dag()` gains `x_scale`/`y_scale` for the layout's shape, `curve_scale`
  (with `variance_scale`/`covariance_scale`) for the curved (co)variance paths,
  and group headings anchored to the diagram rather than to the facet strip.

## Bug fixes

* **Identification of a construct measured at a single occasion by exactly two
  indicators.** The usual referent-indicator and effects-coding schemes leave it
  under-identified. Full tau-equivalence is required. `semTools` has the same
  gap, so semnova routes around it. `identification_scheme()` is the function
  that decides.

* Latent variances and means at the eta layer were free and redundant with the
  contrast layer that carries them, leaving the parameter covariance matrix
  singular. They are fixed to zero.

* A covariate or between-subject factor that varies within a case is now
  refused with an explanatory error, rather than reshaping into missing cells
  and failing much later inside `lavaan` with a message about listwise
  deletion.

* Nested-model comparison no longer deparses the fitted `lavaan` objects into
  its own call, which produced megabytes of output.

## Internals

* R6 is used only for objects that hold state (`Lgc`, `Semnova`, `Hypothesis`,
  `ResidSet`), as fields with no behaviour; everything else is plain functions
  and S3 methods.

* Dependencies dropped from 20 imports to 6 (`R6`, `lavaan`, `semTools`,
  `MASS`, `stats`, `utils`, plus `grid`). The plotting packages (`ggplot2`,
  `ggraph`, `tidygraph`, `semPlot`, `magick`) are optional.

* Arguments to exported functions are validated up front, with errors that name
  the offending value and list the valid ones.

* The package has a test suite (603 tests) where it previously had none.
