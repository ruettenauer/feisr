# feisr 1.2.0

### Major Changes

- `feis()` now has option `weights` to estimate FEIS models with weighted least squares

- Allows estimation of conventional FE models by 'y ~ x | 1'

- Rank deficit columns / NA coefs are now dropped from the model (with warning message)

- Bug fix: Omit intercept from transformed data if intercept = TRUE

### Minor Changes

- `detrend()` allows demeaning by 'slopes = 1'

- Minor documentation and vignette updates

- Omit test R:44:1 for ATLAS as tol seems to be ignored on ATLAS BLAS

### feisr 1.1.4

- Bug fix: pass tol-option to collinearity handling (feis and detrend) introduced in v1.1.2

### feisr 1.1.3

- Bug fix correct order in detrend.

### feisr 1.1.2

- New options se.fit and interval for `predict.feis()`: Compute standard errors and confidence intervals for predicted values.

- New function `detrend()`: Detrends the input data by the predicted values based on the slope parameters within each group.

- Bug fix collinearity handling: now compares equality between predicted x and actual x.

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
