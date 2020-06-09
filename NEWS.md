# feisr 1.1.0

### Major Changes

- Included dplyr `bind_rows()` for faster transformation of large data (depends on dplyr (>= 1.0.0)). Old fall-back code available for dplyr < 1.0.0.

- Added vignette for package use.

- Bug fix `feis()`, `feistest()`, `bsfeistest()`: Standard errors are now computed even if some coefficients are NA.

- `feistest()` and `bsfeistest()`: added `terms` option to perform Hausman / Chi_sq test on subset of coefficients only.

- `feis()`: added `tol` option for detecting linear dependencies in slopes.

- `feis()`: available S3 methods extended by coef, deviance, df.residual, fitted, formula, hatvalues, model.matrix, nobs, predict, residuals, sigma, terms, vcov.

- Added general info page `feisr-package`.


### Minor Changes

- More efficient use of solve / crossprod

- Manual updated, minor corrections.

- Added citation for package.

- Corrected linkage and definition of S3 methods.
