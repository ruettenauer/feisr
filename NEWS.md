# feisr 1.1.0

### Major Changes

- Included dplyr `bind_rows()` for faster tranformation of large data (depends on dplyr (>= 1.0.0)). Old verison available for dplyr < 1.0.0.

- Added vignette.

- Bug fix `feis()`, `feistest()`, `bsfeistest()`: Standard errors can be computed even if some coefficients are NA.

- `feistest()` and `bsfeistest()`: added `terms` option to perform Hausman / Chi_sq test on subset of coefficients.

- `feis()`: added `tol` option for detecting linear dependencies in slopes.

- `feis()`: now compatible with S3 methods for coef, deviance, df.residual, fitted, formula, model.matrix, nobs, predict, residuals, terms, vcov.

- Added general info page `feisr-package`.


### Minor Changes

- Manual updated, minor corrections.

- Added citation for package.

- Corrected linkage / documentation of S3 methods.
