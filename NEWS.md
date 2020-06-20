### feisr 1.1.1

- 

### feisr 1.1.1

- Bug fix for vignette: Add correct suggests version

- Minor documentation updates

# feisr 1.1.0

### Major Changes

- Bug fix `feis()`, `feistest()`, `bsfeistest()`: Vcov / Standard errors are now computed even if some coefficients are NA.

- `feistest()` and `bsfeistest()`: added `terms` option to perform Hausman / Chi_sq test on subset of coefficients only.

- `feis()`: added `tol` option for detecting linear dependencies in slopes.

- `feis()`: available S3 methods extended by coef, deviance, df.residual, fitted, formula, hatvalues, model.matrix, nobs, predict, residuals, sigma, terms, vcov.

- Added general info page `feisr-package`.

- Included dplyr `bind_rows()` for more efficient transformation of large data (depends on dplyr (>= 1.0.0)). Old fall-back code available for dplyr < 1.0.0.

- Added vignette for package use.

### Minor Changes

- More efficient use of solve / crossprod

- Manual updated, minor corrections.

- Added citation for package.

- Corrected linkage and definition of S3 methods.
