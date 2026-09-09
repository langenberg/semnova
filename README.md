# semnova

Latent repeated-measures ANOVA in R. `semnova` fits factorial within- and
between-subject designs as structural equation models, so the dependent
variable and any covariates can be latent rather than manifest composites.

The method is described in Langenberg et al. (2020),
<doi:10.1080/00273171.2020.1803038>.

## Installation

```r
# install.packages("remotes")
remotes::install_github("langenberg/semnova")
```

Plots need a few more packages:

```r
install.packages(c("ggplot2", "ggraph", "tidygraph", "magick"))
```

## Usage

```r
library(semnova)

fit <- semnova(
    data = reading_manifest,
    id = "id",
    dv = "re_fix_time",
    withins = c("grade", "sentence"),
    contrasts_arg = list(grade = "contr.sum", sentence = "contr.sum")
)

fit                     # model description and ANOVA table
summary(fit)            # plus the per-contrast estimates
plot(fit, terms = "grade")
dag(fit)                # path diagram
```

Add `betweens` for a multigroup model, `covariates` for covariates, and
`indicator` to make the dependent variable latent.

## Documentation

```r
vignette("semnova")     # every feature, with worked examples
vignette("glh")         # the general linear hypothesis behind each test
```

## License

GPL (>= 2).
