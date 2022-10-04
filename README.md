# mdlvalr <img src="man/figures/logo.png" align="right" alt="" width="120" /> 

This R package can be used to facilitate validation of MDL pipelines with R.

## Install

Installing the package from a GitHub Enterprise (organization) account requires a few extra arguments, including an [authorization token](https://docs.github.com/en/enterprise-server@3.2/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token). The authorization token is set to expire on 2023-09-30. Please let me know if it is expired or you have any other issues installing from GitHub. 

```{r}
remotes::install_github("knut0297org/mdlvalr", host = "github.umn.edu/api/v3", auth_token = "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")
```
