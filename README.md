
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mdlvalr <img src="man/figures/logo.png" align="right" alt="" width="120" />

This R package can be used to facilitate validation of MDL pipelines
with R. To learn more, please explore the package
[website](https://pages.github.umn.edu/msi/mdlvalr). There, you will
find a list of
[functions](https://pages.github.umn.edu/msi/mdlvalr/reference/index.html),
[articles/vignettes](https://pages.github.umn.edu/msi/mdlvalr/articles/index.html),
and a
[changelog](https://pages.github.umn.edu/msi/mdlvalr/news/index.html).

## Installation

Installing the package from a GitHub Enterprise (organization) account
requires a few extra arguments, including an [authorization
token](https://docs.github.com/en/enterprise-server@3.2/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
The authorization token is set to expire on 2023-09-30. Please let me
know if it is expired or you have any other issues installing from
GitHub.

The package also depends on the `plyranges` R package from Bioconductor.
So make sure that is installed first. Other dependencies are hosted on
CRAN and will be installed automatically if necessary.

``` r
# If you need to install plyranges
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("plyranges")

# Install mdlvalr
remotes::install_github("msi/mdlvalr", host = "github.umn.edu/api/v3", auth_token = "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")
```
